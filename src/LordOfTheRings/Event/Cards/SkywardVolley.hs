module LordOfTheRings.Event.Cards.SkywardVolley (skywardVolley) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Ranged))
import LordOfTheRings.Phase (Phase (CombatPhase))

newtype SkywardVolley = SkywardVolley EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

readyRanged :: PlayerId -> (HeroMatcher, AllyMatcher)
readyRanged pid =
  ( HeroControlledBy (PlayerWithId pid) <> ReadyHero <> HeroWithKeyword Ranged
  , AllyControlledBy (PlayerWithId pid) <> ReadyAlly <> AllyWithKeyword Ranged
  )

-- | As an additional cost to play Skyward Volley, exhaust a ranged character you control.
-- Combat Action: Deal 2 damage to an enemy engaged with a player. Resolve that effect again
-- for each copy of Skyward Volley currently in your discard pile (you may choose different
-- targets).
skywardVolley :: EventCard SkywardVolley
skywardVolley =
  eventWith SkywardVolley Cards.skywardVolley \a ->
    a
      & criteriaL
      .~ DuringPhase CombatPhase
      <> uncurry (CharactersAtLeast 1) (readyRanged a.owner)
      <> exists (EnemyEngagedWith AnyPlayer)

-- | Each time the effect resolves is a later step, carrying how many are still to come.
instance RunMessage SkywardVolley where
  runMessage msg e@(SkywardVolley attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      archers <- uncurry selectCharacters (readyRanged pid)
      chooseOrRunOneM pid do
        questionLabeled "skywardVolley.exhaust"
        causedBy attrs
        targets archers exhaust
      again <- copiesInDiscard pid card
      doStep again msg
      pure e
    DoStep n msg'@(PlayEvent pid card _) | card.id == toId attrs -> do
      enemies <- select (EnemyEngagedWith AnyPlayer)
      chooseOneM pid do
        questionLabeled "skywardVolley.damage"
        causedBy attrs
        targets enemies \eid -> dealDamage attrs eid 2
      when (n > 0) $ doStep (n - 1) msg'
      pure e
    _ -> SkywardVolley <$> runMessage msg attrs
