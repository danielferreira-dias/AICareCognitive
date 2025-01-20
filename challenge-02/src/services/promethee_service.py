from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy import select, insert, update
import numpy as np
from fastapi import HTTPException
from tables.activities import activities_table
from tables.criteria import criteria_table
from tables.weights import weights_table
from tables.decision_matrix import decision_matrix_table
from tables.user import user_table
from tables.user_weights import user_weights_table
from tables.results import results_table


async def get_results(auth0_id: str, db_session: AsyncSession):
    # Step 1: Get user ID based on auth0_id
    user_result = await db_session.execute(
        select(user_table.c.id).where(user_table.c.auth0_user_id == auth0_id)
    )
    user_id = user_result.scalar_one_or_none()
    if not user_id:
        raise HTTPException(status_code=404, detail=f"User not found with auth0 ID: {auth0_id}")

    # Step 2: Fetch user-specific weights
    user_weights_result = await db_session.execute(
        select(user_weights_table.c.criterion_id, user_weights_table.c.weight)
        .where(user_weights_table.c.user_id == user_id)
    )
    user_weights = {criterion_id: weight for criterion_id, weight in user_weights_result.fetchall()}

    # Step 3: Fetch all activities
    activities_result = await db_session.execute(select(activities_table))
    activities = activities_result.fetchall()
    if not activities:
        raise HTTPException(status_code=404, detail="No activities found")

    # Step 4: Fetch all criteria
    criteria_result = await db_session.execute(select(criteria_table))
    criteria = criteria_result.fetchall()
    if not criteria:
        raise HTTPException(status_code=404, detail="No criteria found")

    # Step 5: Fetch the decision matrix scores
    decision_matrix_result = await db_session.execute(
        select(
            decision_matrix_table.c.activity_id,
            decision_matrix_table.c.criterion_id,
            decision_matrix_table.c.score,
        )
    )
    decision_matrix = decision_matrix_result.fetchall()
    if not decision_matrix:
        raise HTTPException(status_code=404, detail="No decision matrix data found")

    # Step 6: Construct the decision matrix
    # Rows: Activities, Columns: Criteria
    activity_ids = [activity[0] for activity in activities]
    criterion_ids = [criterion[0] for criterion in criteria]

    decision_matrix_np = np.zeros((len(activity_ids), len(criterion_ids)))

    activity_id_to_index = {activity_id: idx for idx, activity_id in enumerate(activity_ids)}
    criterion_id_to_index = {criterion_id: idx for idx, criterion_id in enumerate(criterion_ids)}

    for row in decision_matrix:
        activity_idx = activity_id_to_index[row[0]]
        criterion_idx = criterion_id_to_index[row[1]]
        decision_matrix_np[activity_idx][criterion_idx] = row[2]

    # Step 7: Find weights according to disease
    user_weight_vector = np.array([user_weights.get(c_id, 0) for c_id in criterion_ids])

    # Step: 8 ÍNDICES DAS COLUNAS A MAXIMIZAR E MINIMIZAR
    criterios_maximizar = np.array([True, True, True, True, True, False, False, False, False, True, True, True, False])  # Ejemplo: el 1er y 3er criterio son de maximización, el 2do es de minimización

    #################################################
    #             PROMETHEE II METHOD
    ################################################

    # STEP 1: Normalize the decision matrix
    normalized_matrix = decision_matrix_np / np.sqrt((decision_matrix_np ** 2).sum(axis=0))

    # STEP 2: Calculate the difference matrices by criterion (matrices de diferencia x criterio)
    def calcular_matrices_diferencia(D):
        m, n = D.shape
        diferencias_por_criterio = []

        # Calcular las diferencias por cada criterio
        for j in range(n):  # Para cada criterio (columna)
            diferencia_criterio = np.zeros((m, m))  # Inicializar la matriz de diferencia para este criterio
            for i in range(m):
                for k in range(m):
                    if i != k:
                        diferencia_criterio[i, k] = D[i, j] - D[k, j]  # Diferencia por el criterio j
                    else:
                        diferencia_criterio[i, k] = 0  # Las diagonales deben ser 0
            diferencias_por_criterio.append(diferencia_criterio)  # Añadir la matriz de diferencia del criterio j

        return diferencias_por_criterio

    # Calcular matrices de diferencias
    diferencias = calcular_matrices_diferencia(normalized_matrix)

    # STEP 3: Calculate the preference matrix using Usual Type 1 (matrices de preferencia x criterio)
    def calcular_matriz_preferencia(diferencias, criterios_maximizar):
        m = diferencias[0].shape[0]  # Número de alternativas
        preferencias_por_criterio = []

        for idx, diferencia in enumerate(diferencias):
            preferencia_criterio = np.zeros((m, m))
            for i in range(m):
                for j in range(m):
                    if criterios_maximizar[idx]:  # Maximización
                        if diferencia[i, j] > 0:
                            preferencia_criterio[i, j] = 1
                        else:
                            preferencia_criterio[i, j] = 0
                    else:  # Minimización
                        if diferencia[i, j] < 0:
                            preferencia_criterio[i, j] = 1
                        else:
                            preferencia_criterio[i, j] = 0
            preferencias_por_criterio.append(preferencia_criterio)

        return preferencias_por_criterio
    
    # Calcular matrices de preferencias
    preferencias = calcular_matriz_preferencia(diferencias, criterios_maximizar)

    # STEP 4: Calculate the weighted preference index (Matriz ponderada)
    def calcular_indice_preferencia(preferencias, weights):
        m = preferencias[0].shape[0]  # Número de alternativas
        indice_preferencia = np.zeros((m, m))  # Matriz de índice de preferencia

        # Sumar las matrices de preferencia ponderadas
        for idx, preferencia in enumerate(preferencias):
            indice_preferencia += weights[idx] * preferencia  # Multiplicar cada matriz de preferencia por el peso correspondiente

        return indice_preferencia

    # Calcular matrices de preferencias ponderadas
    indice_preferencia = calcular_indice_preferencia(preferencias, user_weight_vector)

    # STEP 5: Calculate the overflows (Flujos de superacion postivos, negativos e netos)
    def calcular_flujos_superacion(indice_preferencia):
        m = indice_preferencia.shape[0]
        flujo_superacion_positivo = np.zeros(m)
        flujo_superacion_negativo = np.zeros(m)

        # Normalizamos por m-1 (en lugar de simplemente sumar)
        for i in range(m):
            flujo_superacion_positivo[i] = np.sum(indice_preferencia[i, :]) / (m - 1)
            flujo_superacion_negativo[i] = np.sum(indice_preferencia[:, i]) / (m - 1)

        # Calcular flujo neto de superación (Φ)
        flujo_neto_superacion = flujo_superacion_positivo - flujo_superacion_negativo

        return flujo_superacion_positivo, flujo_superacion_negativo, flujo_neto_superacion

    # Calcular flujos positivos, negativos, netos
    flujo_superacion_positivo, flujo_superacion_negativo, flujo_neto_superacion = calcular_flujos_superacion(indice_preferencia)

    # -END PROMETHEE METHOD

    # STEP 6: Rank the activities based net flows
    ranked_activities = sorted(zip(activity_ids, flujo_neto_superacion), key=lambda x: x[1], reverse=True)


    # Map activity IDs to their names
    activity_id_to_name = {activity[0]: activity[1] for activity in activities}
    ranked_activities_named = [
        {"activity": activity_id_to_name[activity_id], "score": score}
        for activity_id, score in ranked_activities
    ]

    # Step 12: Save or Update results in the `results_table`
    for activity_id, score in ranked_activities:
        # Check if the result exists for the user and activity
        existing_result = await db_session.execute(
            select(results_table.c.id)
            .where(results_table.c.user_id == user_id)
            .where(results_table.c.activity_id == activity_id)
        )
        existing_result_id = existing_result.scalar_one_or_none()

        if existing_result_id:
            # Update the existing record
            await db_session.execute(
                results_table.update()
                .where(results_table.c.id == existing_result_id)
                .values(score=score)
            )
        else:
            # Insert a new record
            await db_session.execute(
                insert(results_table).values(
                    {"user_id": user_id, "activity_id": activity_id, "score": score}
                )
            )

    # Commit the changes
    await db_session.commit()

    # Step 13: Return ranked activities
    return ranked_activities_named