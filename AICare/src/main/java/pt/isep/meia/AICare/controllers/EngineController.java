package pt.isep.meia.AICare.controllers;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import pt.isep.meia.AICare.application.configs.EngineProperties;

@RestController
@RequestMapping("/api/engine")
public class EngineController {

    private final EngineProperties engineProperties;

    @Autowired
    public EngineController(EngineProperties engineProperties) {
        this.engineProperties = engineProperties;
    }

    @PostMapping()
    public ResponseEntity<String> setEngine(@RequestParam String type) {
        if (!"drools".equalsIgnoreCase(type) && !"prolog".equalsIgnoreCase(type)) {
            return ResponseEntity.badRequest().body("Unknown engine type");
        }
        engineProperties.setType(type);
        return ResponseEntity.ok("Engine type set to " + type);
    }

    @GetMapping()
    public ResponseEntity<String> getEngine() {
        String type = engineProperties.getType();
        if (type == null) {
            return ResponseEntity.badRequest().body("No engine type set");
        }

        return ResponseEntity.ok("Engine is set to " + type);
    }
}
