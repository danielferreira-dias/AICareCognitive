from sqlalchemy import Table, Column, Integer, ForeignKey
from database import metadata

user_weights_table = Table(
    "user_weights",
    metadata,
    Column("id", Integer, primary_key=True),
    Column("user_id", Integer, ForeignKey("users.id"), nullable=False),  # Refers to the users table.
    Column("weight_id", Integer, ForeignKey("weights.id"), nullable=False),  # Refers to the weights table.
    extend_existing=True
)