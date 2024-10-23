<template>
  <div class="w-full h-full bg-white text-black">
    <div class="p-8">
      <div class="flex justify-between items-center mb-6">
        <h1 class="text-2xl rounded-t-lg font-bold text-gray-500">{{ $t('patients.title') }}</h1>
        <!-- Plus Button -->
        <button @click="showAddPatientModal = true"
          class="bg-green-600 text-white px-4 py-2 rounded-lg hover:bg-green-700 transition">
          + {{ $t('patients.addPatient') }}
        </button>
      </div>

      <table class="min-w-full bg-white border rounded-xl border-gray-300">
        <thead>
          <tr class="bg-gray-200">
            <th class="text-left py-3 px-4 text-gray-600 font-bold rounded-tl-lg">{{ $t('patients.table.name') }}</th>
            <th class="text-left py-3 px-4 text-gray-600 font-bold">{{ $t('patients.table.age') }}</th>
            <th class="text-left py-3 px-4 text-gray-600 font-bold">{{ $t('patients.table.gender') }}</th>
            <th class="text-left py-3 px-4 text-gray-600 font-bold rounded-tr-lg">{{ $t('patients.table.actions') }}</th>
          </tr>
        </thead>
        <tbody>
          <template v-for="patient in patients" :key="patient.name">
            <tr class="hover:bg-gray-100 cursor-pointer">
              <td class="py-3 px-4 border-b">{{ patient.name }}</td>
              <td class="py-3 px-4 border-b">{{ patient.age }}</td>
              <td class="py-3 px-4 border-b">{{ patient.gender }}</td>
              <td class="py-3 px-4 border-b">
              <td class="py-3 px-4 border-b flex space-x-2">
                <font-awesome-icon icon="comments" class="text-blue-600 cursor-pointer hover:text-blue-800 transition"
                  @click="goToQuestionPage(patient)" />
                <font-awesome-icon icon="trash-alt" class="text-red-600 cursor-pointer hover:text-red-800 transition"
                  @click.stop="showDeleteConfirmation(patient)" />
              </td>
              </td>
            </tr>
          </template>
        </tbody>
      </table>

      <!-- Add Patient Modal -->
      <div v-if="showAddPatientModal" class="fixed inset-0 bg-black bg-opacity-50 flex justify-center items-center">
        <div class="bg-white p-6 rounded-lg w-96">
          <h2 class="text-xl font-bold mb-4">Add New Patient</h2>
          <form @submit.prevent="addPatient">
            <div class="mb-4">
              <label class="block text-gray-700">Name</label>
              <input v-model="newPatient.name" type="text" class="w-full p-2 border rounded-lg" required />
            </div>
            <div class="mb-4">
              <label class="block text-gray-700">Age</label>
              <input v-model="newPatient.age" type="number" class="w-full p-2 border rounded-lg" required />
            </div>
            <div class="mb-4">
              <label class="block text-gray-700">Gender</label>
              <select v-model="newPatient.gender" class="w-full p-2 border rounded-lg" required>
                <option value="Male">Male</option>
                <option value="Female">Female</option>
                <option value="Other">Other</option>
              </select>
            </div>
            <div class="flex justify-end">
              <button @click="showAddPatientModal = false"
                class="bg-gray-400 text-white px-4 py-2 rounded-lg mr-2">Cancel</button>
              <button type="submit"
                class="bg-green-600 text-white px-4 py-2 rounded-lg hover:bg-green-700 transition">Add</button>
            </div>
          </form>
        </div>
      </div>

      <!-- Delete Confirmation Modal -->
      <div v-if="showDeleteModal" class="fixed inset-0 bg-black bg-opacity-50 flex justify-center items-center">
        <div class="bg-white p-6 rounded-lg w-96">
          <h2 class="text-xl font-bold mb-4">Confirm Delete</h2>
          <p>Are you sure you want to delete {{ patientToDelete?.name }}?</p>
          <div class="flex justify-end mt-4">
            <button @click="showDeleteModal = false"
              class="bg-gray-400 text-white px-4 py-2 rounded-lg mr-2">Cancel</button>
            <button @click="deletePatient"
              class="bg-red-600 text-white px-4 py-2 rounded-lg hover:bg-red-700 transition">Delete</button>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import { getAllPatients, createPatient, deletePatientById } from '../api/services/patientsService';

export default {
  name: 'Idosos',
  data() {
    return {
      selectedPatient: null,
      patients: [],
      showAddPatientModal: false,
      showDeleteModal: false,
      patientToDelete: null,
      newPatient: {
        name: '',
        age: '',
        gender: '',
      }
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
    async addPatient() {
      try {
        const newPatient = await createPatient(this.newPatient);
        this.patients.push(newPatient);
        this.showAddPatientModal = false;
        this.newPatient = { name: '', age: '', gender: '' };
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
      this.$router.push({ name: 'Question', params: { patientId: patient.id } });
    }
  },
  mounted() {
    this.fetchPatients();
  },
};
</script>

<style></style>
