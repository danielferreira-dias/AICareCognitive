import os
from sqlalchemy.ext.asyncio import create_async_engine, AsyncSession
from sqlalchemy.orm import sessionmaker
from sqlalchemy import MetaData
from databases import Database

# Load database URL from environment variable or default to SQLite
DATABASE_URL = os.getenv("DATABASE_URL", "sqlite+aiosqlite:///./aicare.db")

# Create an async engine for async operations
engine = create_async_engine(DATABASE_URL, echo=True)

# Metadata for database schema
metadata = MetaData()

# Create an async session factory
async_session_factory = sessionmaker(
    bind=engine,
    class_=AsyncSession,
    expire_on_commit=False
)

# Use a database instance from the `databases` package (optional, if needed for your project)
database = Database(DATABASE_URL)


# Dependency to provide a database session
async def get_db_session() -> AsyncSession:
    async with async_session_factory() as session:
        yield session