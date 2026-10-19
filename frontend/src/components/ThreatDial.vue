<script setup lang="ts">
import { ref, watch } from 'vue'

const props = defineProps<{ threat: number }>()

// tens wheel on the left, ones wheel on the right
const digits = (t: number) => [Math.floor(t / 10) % 10, t % 10]

// running step counts, so a wheel can turn 9 → 0 forwards instead of spinning all the way back
const steps = ref(digits(props.threat))
watch(() => props.threat, (t) => {
  steps.value = digits(t).map((d, i) => {
    const s = steps.value[i]
    return s + (((d - s) % 10) + 15) % 10 - 5 // shortest way round, in [-5, 4]
  })
})

// clockwise counts down on the left wheel and up on the right one
const wheelAngle = (i: number) => (i === 0 ? -36 : 36) * steps.value[i]
const digitAngle = (i: number, d: number) => (i === 0 ? 36 * d : 180 - 36 * d)
</script>

<template>
  <div class="threat" :class="{ danger: threat >= 40 }" role="img" :aria-label="`Threat ${threat}`">
    <div v-for="i in [0, 1]" :key="i" class="window" :class="i === 0 ? 'left' : 'right'" aria-hidden="true">
      <div class="wheel" :style="{ rotate: `${wheelAngle(i)}deg` }">
        <span v-for="d in 10" :key="d" :style="{ '--a': `${digitAngle(i, d - 1)}deg` }">{{ d - 1 }}</span>
      </div>
    </div>
  </div>
</template>

<style scoped>
.threat {
  width: calc(var(--card-width) * 2);
  aspect-ratio: 75 / 38;
  background-image: url('/images/threat.png');
  background-size: cover;
  border-radius: 5px;
  color: var(--threat-text);
  flex: none;
  position: relative;
  container-type: inline-size;
}
/* the two openings in threat.png (600px wide, so 6px = 1cqw) */
.window { position: absolute; top: 21.3cqw; height: 8.2cqw; width: 16.5cqw; overflow: hidden; }
.left { left: 30.3cqw; --cx: -3.8cqw; }
.right { left: 53.3cqw; --cx: 20.2cqw; --flip: 180deg; }
/* wheels sit outboard of the windows, their rims (digits 36° apart) rolling through them */
.wheel {
  --r: 16cqw;
  position: absolute;
  left: calc(var(--cx) - var(--r));
  top: calc(4.1cqw - var(--r));
  width: calc(2 * var(--r));
  aspect-ratio: 1;
  transition: rotate 0.6s cubic-bezier(0.3, 0.1, 0.3, 1);
}
.wheel span {
  position: absolute;
  left: 50%;
  top: 50%;
  width: 8cqw;
  margin: -4cqw;
  line-height: 8cqw;
  text-align: center;
  font-size: 7cqw;
  transform: rotate(var(--a)) translateX(var(--r)) rotate(var(--flip, 0deg));
  background: linear-gradient(var(--threat-light), var(--threat-light), var(--threat-red), var(--threat-dark));
  background-clip: text;
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
}
.threat.danger .wheel span { background: linear-gradient(var(--threat-danger-light), var(--threat-red), var(--threat-dark)); background-clip: text; -webkit-background-clip: text; }
@media (prefers-reduced-motion: reduce) { .wheel { transition: none; } }
</style>
