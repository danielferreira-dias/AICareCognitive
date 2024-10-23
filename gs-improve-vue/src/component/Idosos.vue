<template>
  <div class="w-full h-full bg-white text-black">
    <div class="p-8">
      <div class="flex justify-between items-center mb-6">
        <h1 class="text-2xl rounded-t-lg font-bold text-gray-500">Patient List</h1>
        <!-- Plus Button -->
        <button @click="showAddPatientModal = true" class="bg-green-600 text-white px-4 py-2 rounded-lg hover:bg-green-700 transition">
          + Add Patient
        </button>
      </div>
      
      <table class="min-w-full bg-white border rounded-xl border-gray-300">
        <thead>
          <tr class="bg-gray-200">
            <th class="text-left py-3 px-4 text-gray-600 font-bold rounded-tl-lg">Name</th>
            <th class="text-left py-3 px-4 text-gray-600 font-bold">Age</th>
            <th class="text-left py-3 px-4 text-gray-600 font-bold rounded-tr-lg">Gender</th>
          </tr>
        </thead>
        <tbody>
          <template v-for="patient in patients" :key="patient.name">
            <tr class="hover:bg-gray-100 cursor-pointer" @click="togglePatient(patient)">
              <td class="py-3 px-4 border-b">{{ patient.name }}</td>
              <td class="py-3 px-4 border-b">{{ patient.age }}</td>
              <td class="py-3 px-4 border-b">{{ patient.gender }}</td>
            </tr>

            <!-- Slide Down Additional Patient Info -->
            <tr v-if="selectedPatient && selectedPatient.id.trim().toLowerCase() === patient.id.trim().toLowerCase()"
              class="transition-all ease-in duration-300">
              <td colspan="3" class="py-4 px-4 bg-gray-50">
                <div class="text-gray-600 flex flex-row w-full justify-evenly">
                  <p>Start a Survey about {{ selectedPatient.name }}.</p>
                  <div class="w-14 h-fit bg-purple-900 text-white rounded-lg text-center">Start</div>
                </div>
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
              <button @click="showAddPatientModal = false" class="bg-gray-400 text-white px-4 py-2 rounded-lg mr-2">Cancel</button>
              <button type="submit" class="bg-green-600 text-white px-4 py-2 rounded-lg hover:bg-green-700 transition">Add</button>
            </div>
          </form>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import { getAllPatients, createPatient } from '../api/services/patientsService';

export default {
  name: 'Idosos',
  data() {
    return {
      selectedPatient: null,
      patients: [],
      showAddPatientModal: false,
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
    togglePatient(patient) {
      this.selectedPatient = patient;
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
    }
  },
  mounted() {
    this.fetchPatients();
  },
};
</script>

<style></style>
