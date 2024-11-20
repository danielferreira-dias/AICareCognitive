package pt.isep.meia.AICare.controllers;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import pt.isep.meia.AICare.application.configs.EngineProperties;
import pt.isep.meia.AICare.domain.dtos.EngineDto;

@RestController
@RequestMapping("/api/engine")
public class EngineController {

    private final EngineProperties engineProperties;

    @Autowired
    public EngineController(EngineProperties engineProperties) {
        this.engineProperties = engineProperties;
    }

    @PostMapping()
    public ResponseEntity<EngineDto> setEngine(@RequestBody EngineDto requestDto) {
        if (!"drools".equalsIgnoreCase(requestDto.engine) && !"prolog".equalsIgnoreCase(requestDto.engine)) {
            return ResponseEntity.badRequest().body(requestDto);
        }
        engineProperties.setType(requestDto.engine);
        return ResponseEntity.ok(requestDto);
    }

    @GetMapping()
    public ResponseEntity<EngineDto> getEngine() {
        String type = engineProperties.getType();
        if (type == null) {
            return ResponseEntity.badRequest().body(new EngineDto());
        }

        return ResponseEntity.ok(new EngineDto(type));
    }
}
