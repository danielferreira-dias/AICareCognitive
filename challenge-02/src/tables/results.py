from sqlalchemy import Table, Column, Integer, Float, ForeignKey
from database import metadata

results_table = Table(
    "results",
    metadata,
    Column("id", Integer, primary_key=True),
    Column("user_id", Integer, ForeignKey("users.id"), nullable=False),
    Column("activity_id", Integer,ForeignKey("activities.id"), nullable=False),
    Column("score", Float, nullable=False),
    extend_existing=True
)
