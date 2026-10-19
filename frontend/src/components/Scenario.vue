<script setup lang="ts">
import { computed } from 'vue'
import { GameData, Scenario, Choice, Quest, Player } from './types/Game'
import Card from './Card.vue'
import Location from './Location.vue'

const emit = defineEmits<{ reset: [], choose: [Choice] }>()
const props = defineProps<{ you: Player, data: GameData, scenario: Scenario }>()

const quest = computed<Quest>(() => {
  return Object.values(props.data.game.entities.quests)[0]
})

const question = computed(() => {
  return props.data.game.question[props.you.id]
})

const enemies = computed(() => {
  return Object.values(props.data.game.entities.enemies).filter((enemy) => enemy.placement.tag === "StagingArea")
})

const activeLocation = computed(() => {
  return Object.values(props.data.game.entities.locations).find((location) => location.placement.tag === "ActiveLocation")
})

const locations = computed(() => {
  return Object.values(props.data.game.entities.locations).filter((location) => location.placement.tag === "StagingArea")
})

</script>

<template>
  <div class="scenario-main">
    <div class="scenario">
      <div class="deck">
        <div class="deck-size">{{scenario.encounterDeck.length}}</div>
      </div>
      <Card v-if="quest" :card="{ id: quest.cardId, cardCode: quest.cardCode }" :side="quest.flipped ? 'B' : 'A'" :progress="quest.tokens.Progress" :data="data" @choose="$emit('choose', $event)" rotated :target="{ tag: 'QuestTarget', contents: quest.id }" :you="you" />
      <Location v-if="activeLocation" :location="activeLocation" :you="you" :data="data" @choose="$emit('choose', $event)" />
    </div>

    <div class="staging">
      <Card v-for="enemy in enemies" :key="enemy.id" :card="{ id: enemy.cardId, cardCode: enemy.cardCode }" :target="{ tag: 'EnemyTarget', contents: enemy.id }" :data="data" @choose="$emit('choose', $event)" :you="you" />
      <Location v-for="location in locations" :key="location.id" :location="location" :you="you" :data="data" @choose="$emit('choose', $event)" />
    </div>
  </div>
</template>

<style scoped lang="scss">
.deck {
  width: $card-width;
  aspect-ratio: $card-aspect;
  height: fit-content;
  background-image: url('/images/encounter.jpg');
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

.staging {
  display: flex;
  gap: $gap;
}

.scenario {
  display: flex;
  gap: $gap;
}

.scenario-main {
  display: flex;
  gap: $gap;
  flex-direction: column;
}
</style>
