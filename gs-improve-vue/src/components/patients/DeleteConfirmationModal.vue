<template>
    <div class="fixed inset-0 bg-black bg-opacity-50 flex justify-center items-center">
        <div class="bg-white p-6 rounded-lg w-96">
            <h2 class="text-xl font-bold mb-4">{{ $t('patients.deletePatient.title') }}</h2>
            <p>{{ $t('patients.deletePatient.message', { name: patient.name }) }}</p>
            <div class="flex justify-end mt-4">
                <button @click="$emit('closeModal')" class="bg-gray-400 text-white px-4 py-2 rounded-lg mr-2">
                    {{ $t('patients.deletePatient.cancel') }}
                </button>
                <button @click="$emit('confirmDelete', patient)"
                    class="bg-red-600 text-white px-4 py-2 rounded-lg hover:bg-red-700 transition">
                    {{ $t('patients.deletePatient.confirm') }}
                </button>
            </div>
        </div>
    </div>
</template>

<script>
export default {
    name: 'DeleteConfirmationModal',
    props: {
        patient: {
            type: Object,
            required: true
        }
    }
};
</script>