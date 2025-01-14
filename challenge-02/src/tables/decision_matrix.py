from sqlalchemy import Table, Column, Integer, Float, ForeignKey
from database import metadata

decision_matrix_table = Table(
    "decision_matrix",
    metadata,
    Column("id", Integer, primary_key=True),
    Column("activity_id", Integer, ForeignKey("activities.id"), nullable=False),  # Refers to the activities table.
    Column("criterion_id", Integer, ForeignKey("criteria.id"), nullable=False),  # Refers to the criteria table.
    Column("score", Float, nullable=False),  # Score assigned to the activity for this criterion.
)