export class CreatePatientRequestDto {
    constructor({ name, age, gender }) {
        this.name = name;
        this.age = age;
        this.gender = gender;
    }
}

export class PatientDto {
    constructor({ id, name, age, gender }) {
        this.id = id;
        this.name = name;
        this.age = age;
        this.gender = gender;
    }
}
