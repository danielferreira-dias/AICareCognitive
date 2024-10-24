package pt.isep.meia.AICare.infrastructure.gateways;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.var;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import pt.isep.meia.AICare.domain.model.Evidence;
import pt.isep.meia.AICare.domain.model.Result;
import pt.isep.meia.AICare.infrastructure.gateways.dtos.PrologResultDto;

import java.util.List;
import java.util.Objects;
import java.util.UUID;

@Service
public class PrologGateway {

    private final String serverUrl;
    private final RestTemplate restTemplate;
    private final ObjectMapper objectMapper;

    @Autowired
    public PrologGateway(
            RestTemplate restTemplate,
            ObjectMapper objectMapper) {
        this.objectMapper = objectMapper;
        this.serverUrl = "http://localhost:8081";
        this.restTemplate = restTemplate;
    }

    public Result getNextQuestion(UUID surveyId, int order) {
        var endpoint = serverUrl + "/next_question";
        var response = restTemplate.getForEntity(endpoint, PrologResultDto.class);
        return Result.fromPrologResult(surveyId, Objects.requireNonNull(response.getBody()), order);
    }

    public boolean postAnswer(String evidence, String answer) {
        var endpoint = serverUrl + "/answer";
        var headers = new HttpHeaders();
        headers.set("Content-Type", "application/json; charset=UTF-8");

        var json = objectMapper.createObjectNode();
        json.put("evidence", evidence);
        json.put("answer", answer);

        var entity = new HttpEntity<>(json.toString(), headers);

        var response = restTemplate.postForEntity(endpoint, entity, String.class);
        return response.getStatusCode().equals(HttpStatus.OK);
    }

    public boolean postBulkAnswers(List<Evidence> evidences) {
        var endpoint = serverUrl + "/bulk_answer";
        var headers = new HttpHeaders();
        headers.set("Content-Type", "application/json; charset=UTF-8");

        var jsonArray = objectMapper.createArrayNode();
        for (var evidence : evidences) {
            var json = objectMapper.createObjectNode();
            json.put("evidence", evidence.getQuestion().getText());
            json.put("answer", evidence.getAnswer().getResponse());
            jsonArray.add(json);
        }

        var entity = new HttpEntity<>(jsonArray.toString(), headers);

        var response = restTemplate.exchange(endpoint, HttpMethod.POST, entity, String.class);
        return response.getStatusCode().equals(HttpStatus.OK);
    }
}
