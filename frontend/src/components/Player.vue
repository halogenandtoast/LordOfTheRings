<script setup lang="ts">
import { Player, GameData, Choice } from '@/types/Game'
import Card from './Card.vue'
defineProps<{ player: Player, data: GameData }>()
const emit = defineEmits<{ choose: [Choice] }>()
</script>

<template>
  <div class="player">
    <div class="heroes">
      <Card v-for="hero in player.heroes" :key="hero.id" :card="hero.card" :resources="hero.resources" :data="data" @choose="$emit('choose')" />
    </div>
    <div class="player-main">
      <div class="discard">
      </div>
      <div class="deck">
        <div class="deck-size">{{player.deck.length}}</div>
      </div>
      <div class="hand">
        <Card v-for="card in player.hand" :key="card.id" :card="card" :data="data" @choose="$emit('choose', $event)" />
      </div>
      <div class="threat">
        <span>{{player.threat}}</span>
      </div>
    </div>
  </div>
</template>

<style scoped lang="scss">
.player {
  align-self: flex-end;
  gap: $gap;
  display: flex;
  flex-direction: column;
}

.heroes {
  display: flex;
  gap: $gap;
}

.hand {
  display: flex;
  width: $card-width;
  flex: 1;
  gap: $gap;
}

.discard {
  height: $card-width;
  aspect-ratio: 1063/756;
}

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

.player-main {
  display: flex;
  flex-direction: row;
  gap: $gap;
  align-items: center;
}

.threat {
  width: calc($card-width * 2);
  aspect-ratio: 75/38;
  background-image: url('/images/threat.png');
  background-size: cover;
  display: grid;
  place-items: center;
  border-radius: 5px;
  color: rgba(255, 255, 255);
  letter-spacing: 25px;
  text-indent: 25px;
  span {
    background: -webkit-linear-gradient(#E8DDA6, #E8DDA6, #E93323, #822017);
    -webkit-background-clip: text;
    background-clip: text;
    -webkit-text-fill-color: transparent;
  }
}

</style>
