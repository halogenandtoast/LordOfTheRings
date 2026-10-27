module LordOfTheRings.Location.Cards.GoblinGate (goblinGate) where

import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Keyword (Keyword (Ambush))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype GoblinGate = GoblinGate LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

goblinGate :: LocationCard GoblinGate
goblinGate = location GoblinGate Cards.goblinGate

-- | While Goblin Gate is the active location, the first enemy revealed from the encounter
-- deck each round gains ambush. If that enemy engages a player, it makes an immediate
-- attack (deal and resolve a shadow card).
-- ponytail: "that enemy" is the one Goblin Gate gave ambush to this round, read from the
-- modifier; nothing else in the game grants ambush.
instance HasAbilities GoblinGate where
  getAbilities (GoblinGate a) =
    [ limited (GroupLimit PerRound 1) $ restricted a 1 active $ forced $ EnemyRevealedWindow When
    , restricted a 2 active $ forced $ EnemyWithModifierEngagedWindow After (GainKeyword Ambush)
    ]
   where
    active = exists (LocationWithId a.id <> IsActiveLocation)

instance RunMessage GoblinGate where
  runMessage msg l@(GoblinGate a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ [eid | Window _ (Revealed (EnemyTarget eid)) <- ws] \eid -> roundModifier a eid (GainKeyword Ambush)
      pure l
    UseCardAbility _ (isSource a -> True) 2 ws -> do
      for_ [(eid, pid) | Window _ (EnemyEngaged eid pid) <- ws] \(eid, pid) ->
        whenM (elem (GainKeyword Ambush) <$> getModifiers eid) $ immediateAttack pid eid
      pure l
    _ -> GoblinGate <$> runMessage msg a
