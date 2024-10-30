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
import pt.isep.meia.AICare.application.configs.PrologServerProperties;
import pt.isep.meia.AICare.domain.dtos.PrologJustificationDto;
import pt.isep.meia.AICare.domain.model.Evidence;
import pt.isep.meia.AICare.domain.model.Justification;
import pt.isep.meia.AICare.domain.model.JustificationTypeEnum;
import pt.isep.meia.AICare.domain.model.Result;
import pt.isep.meia.AICare.infrastructure.gateways.dtos.PrologResultDto;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.stream.Collectors;

@Service
public class PrologGateway {

    private final String serverUrl;
    private final RestTemplate restTemplate;
    private final ObjectMapper objectMapper;

    @Autowired
    public PrologGateway(
            RestTemplate restTemplate,
            ObjectMapper objectMapper,
            PrologServerProperties prologServerProperties) {
        this.objectMapper = objectMapper;
        this.serverUrl = prologServerProperties.getUrl();
        this.restTemplate = restTemplate;
    }

    public Result getNextQuestion(UUID surveyId, int order) {
        var endpoint = serverUrl + "/next_question";
        var response = restTemplate.getForEntity(endpoint, PrologResultDto.class);
        return Result.fromPrologResult(surveyId, Objects.requireNonNull(response.getBody()), order);
    }

    public List<Justification> getWhy(String activity) {
        var endpoint = serverUrl + "/get_why/" + activity;
        var response = restTemplate.getForEntity(endpoint, PrologJustificationDto.class);
        return response.getBody().getJustifications().stream()
                .map(justificationList -> new Justification(JustificationTypeEnum.why, String.join(".", justificationList), new ArrayList<>()))
                .collect(Collectors.toList());
    }

    public List<Justification> getWhyNot(String activity) {
        var endpoint = serverUrl + "/get_why_not/" + activity;
        var response = restTemplate.getForEntity(endpoint, PrologJustificationDto.class);
        return response.getBody().getJustifications().stream()
                .map(justificationList -> new Justification(JustificationTypeEnum.whynot, String.join(".", justificationList), new ArrayList<>()))
                .collect(Collectors.toList());
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
