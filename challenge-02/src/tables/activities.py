from sqlalchemy import Table, Column, Integer, Float, String
from database import metadata

activities_table = Table(
    "activities",
    metadata,
    Column("id", Integer, primary_key=True),
    Column("name", String, nullable=False),
    extend_existing=True
)
