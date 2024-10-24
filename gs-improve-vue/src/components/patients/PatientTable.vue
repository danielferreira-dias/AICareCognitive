<template>
    <table class="min-w-full bg-white border rounded-xl border-gray-300">
        <thead>
            <tr class="bg-gray-200">
                <th class="text-left py-3 px-4 text-gray-600 font-bold rounded-tl-lg">{{ $t('patients.table.name') }}
                </th>
                <th class="text-left py-3 px-4 text-gray-600 font-bold">{{ $t('patients.table.age') }}</th>
                <th class="text-left py-3 px-4 text-gray-600 font-bold">{{ $t('patients.table.gender') }}</th>
                <th class="text-left py-3 px-4 text-gray-600 font-bold rounded-tr-lg">{{ $t('patients.table.actions') }}
                </th>
            </tr>
        </thead>
        <tbody>
            <template v-for="patient in patients" :key="patient.name">
                <tr class="hover:bg-gray-100 cursor-pointer">
                    <td class="py-3 px-4 border-b">{{ patient.name }}</td>
                    <td class="py-3 px-4 border-b">{{ patient.age }}</td>
                    <td class="py-3 px-4 border-b">{{ $t(`patients.table.${patient.gender.toLowerCase()}`) }}</td>
                    <td class="py-3 px-4 border-b flex space-x-2">
                        <font-awesome-icon icon="comments"
                            class="text-blue-600 cursor-pointer hover:text-blue-800 transition"
                            @click="$emit('navigateToSurvey', patient)" />
                        <font-awesome-icon icon="trash-alt"
                            class="text-red-600 cursor-pointer hover:text-red-800 transition"
                            @click.stop="$emit('deletePatient', patient)" />
                    </td>
                </tr>
            </template>
        </tbody>
    </table>
</template>

<script>
export default {
    name: 'PatientTable',
    props: {
        patients: {
            type: Array,
            required: true
        }
    }
};
</script>