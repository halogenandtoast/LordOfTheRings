<script setup lang="ts">
import { computed } from 'vue'
import  { Choice, GameData, Player } from '@/types/Game'

const props = defineProps<{ ability: Ability, choice: Choice, you: Player, data: GameData }>()
const emit = defineEmits<{ choose: [Choice] }>()

const labels: Record<string, string> = {
  ActionAbility: 'Action',
  ResponseAbility: 'Response',
  ForcedAbility: 'Forced',
  SilentForcedAbility: 'Forced',
}

const label = computed(() => labels[props.ability.abilityType.tag] ?? 'Use')
</script>

<template>
  <button @click="$emit('choose', choice)">{{ label }}</button>
</template>
