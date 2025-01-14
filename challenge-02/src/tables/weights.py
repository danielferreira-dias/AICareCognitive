from sqlalchemy import Table, Column, Integer, Float, ForeignKey, String
from database import metadata

weights_table = Table(
    "weights",
    metadata,
    Column("id", Integer, primary_key=True),
    Column("disease", String, nullable=False),
    Column("criterion_id", Integer, ForeignKey("criteria.id"), nullable=False),
    Column("weight", Float, nullable=False),
)