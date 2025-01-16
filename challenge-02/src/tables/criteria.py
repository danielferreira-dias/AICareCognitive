from sqlalchemy import Table, Column, Integer, String
from database import metadata

criteria_table = Table(
    "criteria",
    metadata,
    Column("id", Integer, primary_key=True),
    Column("name", String, nullable=False),
    extend_existing=True
)