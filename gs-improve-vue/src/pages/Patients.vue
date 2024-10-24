<template>
  <div class="w-full h-full bg-white text-black">
    <div class="p-8">
      <div class="flex justify-between items-center mb-6">
        <h1 class="text-2xl rounded-t-lg font-bold text-gray-500">{{ $t('patients.title') }}</h1>
        <button @click="showAddPatientModal = true"
          class="bg-green-600 text-white px-4 py-2 rounded-lg hover:bg-green-700 transition">
          + {{ $t('patients.addPatient.addButton') }}
        </button>
      </div>

      <PatientTable :patients="patients" @navigateToSurvey="goToQuestionPage" @deletePatient="showDeleteConfirmation" />

      <AddPatientModal v-if="showAddPatientModal" @addPatient="addPatient" @closeModal="showAddPatientModal = false" />

      <DeleteConfirmationModal v-if="showDeleteModal" :patient="patientToDelete" @confirmDelete="deletePatient"
        @closeModal="showDeleteModal = false" />
    </div>
  </div>
</template>

<script>
import { getAllPatients, createPatient, deletePatientById } from '../api/services/patientService';
import PatientTable from '../components/patients/PatientTable.vue';
import AddPatientModal from '../components/patients/AddPatientModal.vue';
import DeleteConfirmationModal from '../components/patients/DeleteConfirmationModal.vue';

export default {
  name: 'Patients',
  components: {
    PatientTable,
    AddPatientModal,
    DeleteConfirmationModal
  },
  data() {
    return {
      selectedPatient: null,
      patients: [],
      showAddPatientModal: false,
      showDeleteModal: false,
      patientToDelete: null
    };
  },
  methods: {
    async fetchPatients() {
      try {
        const patients = await getAllPatients();
        this.patients = patients;
      } catch (error) {
        console.error('Error fetching patients:', error);
      }
    },
    async addPatient(newPatient) {
      try {
        const createdPatient = await createPatient(newPatient);
        this.patients.unshift(createdPatient); // Add new patient to the beginning of the array
        this.showAddPatientModal = false;
      } catch (error) {
        console.error('Error adding patient:', error);
      }
    },
    showDeleteConfirmation(patient) {
      this.patientToDelete = patient;
      this.showDeleteModal = true;
    },
    async deletePatient() {
      if (!this.patientToDelete) return;
      try {
        await deletePatientById(this.patientToDelete.id);
        this.patients = this.patients.filter(p => p.id !== this.patientToDelete.id);
        this.showDeleteModal = false;
        this.patientToDelete = null;
      } catch (error) {
        console.error('Error deleting patient:', error);
      }
    },
    goToQuestionPage(patient) {
      this.$router.push({ name: 'Surveys', params: { patientId: patient.id } });
    }
  },
  mounted() {
    this.fetchPatients();
  }
};
</script>
