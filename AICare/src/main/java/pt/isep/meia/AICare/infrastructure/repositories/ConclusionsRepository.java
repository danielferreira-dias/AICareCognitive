package pt.isep.meia.AICare.infrastructure.repositories;

import org.springframework.data.jpa.repository.JpaRepository;
import pt.isep.meia.AICare.domain.entities.Conclusion;

import java.util.UUID;

public interface ConclusionsRepository extends JpaRepository<Conclusion, UUID> {
}
