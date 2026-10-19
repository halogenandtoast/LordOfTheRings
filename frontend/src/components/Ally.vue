<script setup lang="ts">
import { computed } from 'vue'
import  { Choice, Ally, GameData, Player } from '@/types/Game'
import Card from './Card.vue'
import Ability from './Ability.vue'

const props = defineProps<{ ally: Ally, you: Player, data: GameData }>()
const emit = defineEmits<{ choose: [Choice] }>()

function toChoices(q) {
  if (!q) return []
  switch(q.tag) {
    case 'ChooseOne':
      return q.choices
    case 'ChooseOneAtATime':
      return q.choices
  }

  return []
}

const choices = computed(() => {
  return toChoices(props.data.game.question[props.you.id])
})

const abilityChoices = computed(() => {
  const xs = choices.value.filter((c) => {
    if (c.tag !== 'AbilityLabel') return false
    return c.ability.source.contents === props.ally.id
  })

  return xs
})


</script>

<template>
  <div class="ally" :data-index="ally.cardId">
    <Card :card="{ id: ally.cardId, cardCode: ally.cardCode }" :target="{ tag: 'AllyTarget', contents: ally.id }" :data="data" @choose="$emit('choose', $event)" :you="you" />
    <Ability v-for="choice in abilityChoices" :choice="choice" :ability="choice.ability" :key="`${choice.ability.source.contents}${choice.ability.index}`" :data="data" :you="you" @choose="$emit('choose', $event)" />
  </div>
</template>
