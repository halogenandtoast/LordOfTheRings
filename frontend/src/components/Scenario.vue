<script setup lang="ts">
import { computed } from 'vue'
import { GameData, Scenario, Choice } from './types/Game'
import Card from './Card.vue'

const emit = defineEmits<{ reset: [], choose: [Choice] }>()
const props = defineProps<{ data: GameData, scenario: Scenario }>()

const quest = computed(() => {
  return props.scenario.quest
})

const question = computed(() => {
  return props.data.game.players[props.data.playerId].question
})

const questChoice = computed(() => {
  const currentQuestion = question.value
  if (!currentQuestion) return null

  switch (currentQuestion.tag) {
    case "ChooseOne":
      return currentQuestion.contents.find((choice: Choice) => choice.tag === "QuestChoice")
    default:
      return null
  }
})

</script>

<template>
  <div>Scenario</div>
  <div class="deck">
    <div class="deck-size">{{scenario.encounterDeck.length}}</div>
  </div>
  <Card v-if="quest" :card="quest.card" :side="quest.side" :data="data" @choose="$emit('choose', $event)" rotated :currentChoice="questChoice" />
</template>

<style scoped lang="scss">
.deck {
  width: $card-width;
  aspect-ratio: 756/1063;
  background-image: url('/images/player.jpg');
  background-size: cover;
  display: grid;
  place-items: center;
  border-radius: 5px;
}

.deck-size {
  color: white;
  background: rgba(0, 0, 0, 0.8);
  padding: calc($card-width / 10);
  border-radius: calc($card-width / 5);
  border: 1px solid white;
  font-size: $font-small;
}
</style>
