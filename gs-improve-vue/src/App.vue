<template>
  <div
    class="flex flex-col md:flex-row items-center h-screen bg-gradient-to-t from-zinc-900 via-zinc-800 to-zinc-700">
    <Navbar/>
    <Idosos/>
    <!-- <Question/> -->
  </div>
</template>

<script>
import Question from './component/Question.vue';
import Navbar from './component/Navbar.vue';
import Idosos from './component/Idosos.vue';
export default {
  components:{
    Question,
    Navbar,
    Idosos
  },
  data() {
    return
  },
};
</script>

