<script setup lang="ts">
import { computed } from 'vue'
import  { Choice, Location, GameData, Player } from '@/types/Game'
import Card from './Card.vue'
import Ability from './Ability.vue'

const props = defineProps<{ location: Location, you: Player, data: GameData }>()
const emit = defineEmits<{ choose: [Choice] }>()

function toChoices(q) {
  if (!q) return []
  switch(q.tag) {
    case 'ChooseOne':
      return q.contents
    case 'ChooseOneAtATime':
      return q.contents
  }

  return []
}

const choices = computed(() => {
  return toChoices(props.data.game.players[props.you.id].question)
})

const abilityChoices = computed(() => {
  return choices.value.filter((c) => {
    if (c.tag !== 'UseAbilityLabel') return false
    return c.ability.target.contents === props.location.id
  })
})


</script>

<template>
  <div class="location">
    <Card :card="location.card" :data="data" @choose="$emit('choose', $event)" :data-index="location.card.id" :you="you" :progress="location.progress" />
    <Ability v-for="choice in abilityChoices" :choice="choice" :ability="choice.ability" :key="`${choice.ability.target.contents}${choice.ability.index}`" :data="data" :you="you" @choose="$emit('choose', $event)" />
  </div>
</template>
