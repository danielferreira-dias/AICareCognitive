from sqlalchemy import Table, Column, Integer, Float, ForeignKey
from database import metadata

user_weights_table = Table(
    "user_weights",
    metadata,
    Column("id", Integer, primary_key=True),
    Column("user_id", Integer, ForeignKey("users.id"), nullable=False),  # Refers to the users table
    Column("criterion_id", Integer, ForeignKey("criteria.id"), nullable=False),  # Refers to criteria
    Column("weight", Float, nullable=False),
    extend_existing=True
)
