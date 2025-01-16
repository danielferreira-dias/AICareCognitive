from sqlalchemy import Table, Column, Integer, String, ForeignKey
from database import metadata

disease_predictions_table = Table(
    "disease_predictions",
    metadata,
    Column("id", Integer, primary_key=True),
    Column("user_id", Integer, ForeignKey("users.id"), nullable=False),  # Refers to the users table.
    Column("prediction", String, nullable=False),
    extend_existing=True # "Alzheimer", "Parkinson", "Both", or "None".
)