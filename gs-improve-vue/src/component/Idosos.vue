<template>
  <div class="w-full h-full bg-white text-black">
    <div class="p-8">
      <h1 class="text-2xl rounded-t-lg font-bold text-gray-500 mb-6">User List</h1>
      <table class="min-w-full bg-white border rounded-xl border-gray-300">
        <thead>
          <tr class="bg-gray-200">
            <th class="text-left py-3 px-4 text-gray-600 font-bold rounded-tl-lg">Name</th>
            <th class="text-left py-3 px-4 text-gray-600 font-bold">Age</th>
            <th class="text-left py-3 px-4 text-gray-600 font-bold rounded-tr-lg">Sex</th>
          </tr>
        </thead>
        <tbody>
          <template v-for="patient in patients" :key="patient.name">
            <tr class="hover:bg-gray-100 cursor-pointer" @click="toggleUser(patient)">
              <td class="py-3 px-4 border-b">{{ patient.name }}</td>
              <td class="py-3 px-4 border-b">{{ patient.age }}</td>
              <td class="py-3 px-4 border-b">{{ patient.gender }}</td>
            </tr>

            <!-- Slide Down Additional User Info -->
            <tr v-if="selectedUser && selectedUser.name.trim().toLowerCase() === patient.name.trim().toLowerCase()"
              class="transition-all ease-in duration-300">
              <td colspan="3" class="py-4 px-4 bg-gray-50">
                <div class="text-gray-600 flex flex-row w-full justify-evenly">
                  <p> Start a Survey about {{ selectedUser.name }}.</p>
                  <div class="w-14 h-fit bg-purple-900 text-white rounded-lg text-center">Start</div>
                </div>
              </td>
            </tr>
          </template>
        </tbody>
      </table>
    </div>
  </div>
</template>

<script>
import { getAllPatients } from '../api/services/patientsService';

export default {
  name: 'Idosos',
  data() {
    return {
      selectedPatient: null,
      patients: [],
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
    toggleUser(user) {
      this.selectedUser = user;
      console.log(this.selectedUser)
    }
  },
  mounted() {
    this.fetchPatients();
  },
};
</script>

<style></style>
