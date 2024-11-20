<template>
    <div class="fixed inset-0 bg-black bg-opacity-50 flex justify-center items-center">
        <div class="bg-white p-6 rounded-lg w-96">
            <h2 class="text-xl font-bold mb-4">{{ $t('patients.addPatient.title') }}</h2>
            <form @submit.prevent="onAddPatient">
                <div class="mb-4">
                    <label class="block text-gray-700">{{ $t('patients.addPatient.name') }}</label>
                    <input v-model="newPatient.name" type="text" class="w-full p-2 border rounded-lg" required />
                </div>
                <div class="mb-4">
                    <label class="block text-gray-700">{{ $t('patients.addPatient.age') }}</label>
                    <input v-model.number="newPatient.age" type="number" min="1" max="200"
                        class="w-full p-2 border rounded-lg" required />
                </div>
                <div class="mb-4">
                    <label class="block text-gray-700">{{ $t('patients.addPatient.gender') }}</label>
                    <select v-model="newPatient.gender" class="w-full p-2 border rounded-lg" required>
                        <option value="Male">{{ $t('patients.addPatient.male') }}</option>
                        <option value="Female">{{ $t('patients.addPatient.female') }}</option>
                        <option value="Other">{{ $t('patients.addPatient.other') }}</option>
                    </select>
                </div>
                <div class="flex justify-end">
                    <button @click="$emit('closeModal')" class="bg-gray-400 text-white px-4 py-2 rounded-lg mr-2">
                        {{ $t('patients.addPatient.cancel') }}
                    </button>
                    <button type="submit"
                        class="bg-green-600 text-white px-4 py-2 rounded-lg hover:bg-green-700 transition">
                        {{ $t('patients.addPatient.confirm') }}
                    </button>
                </div>
            </form>
        </div>
    </div>
</template>

<script>
export default {
    data() {
        return {
            newPatient: {
                name: '',
                age: '',
                gender: ''
            }
        };
    },
    methods: {
        onAddPatient() {
            if (this.newPatient.age < 1 || this.newPatient.age > 200) {
                alert("Age must be between 1 and 200.");
                return;
            }
            this.$emit('addPatient', this.newPatient);
            this.newPatient = { name: '', age: '', gender: '' };
        }
    }
};
</script>