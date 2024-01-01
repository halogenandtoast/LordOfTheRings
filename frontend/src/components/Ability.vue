<script setup lang="ts">
import  { Choice, GameData, Player } from '@/types/Game'

const props = defineProps<{ ability: Ability, choice: Choice, you: Player, data: GameData }>()
const emit = defineEmits<{ choose: [Choice] }>()
</script>

<template>
  <button :alt="ability.text" @click="$emit('choose', choice)">{{ability.kind.tag}}</button>
</template>
