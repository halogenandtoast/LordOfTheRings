module LordOfTheRings.Enemy.Cards.GollumASitE (gollumASitE) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith, StagingArea))
import LordOfTheRings.Trait (Trait (Gollum))

-- | Sméagol's enemy side: his owner's card still, in play engaged. Flipped, he is the hero
-- of "LordOfTheRings.Hero.Cards.Smeagol".
newtype GollumASitE = GollumASitE EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

gollumASitE :: EnemyCard GollumASitE
gollumASitE = enemy GollumASitE Cards.gollumASitE

-- | Immune to non-Gollum card effects. Sméagol, his own other side, is the only Gollum
-- player card, so every player card's effect is a non-Gollum one; of the encounter cards,
-- Stinker's are Gollum's.
-- Gollum engages the first player: no other player can engage him.
-- Defeated, he stays in play for his Forced effect to flip him.
instance HasModifiersFor GollumASitE where
  getModifiersFor (GollumASitE a) = do
    lead <- getLead
    others <- filter (/= lead) <$> getPlayerOrder
    modifySelf a
      $ [ImmuneToPlayerCardEffects, ImmuneToEncounterCardEffectsWithout Gollum, EngagesOnly lead, StaysInPlayWhenDefeated]
      <> map CannotEngagePlayer others

{- | While Gollum is in play, he engages the first player (MEC77 rules): he enters play
engaged with his owner, then engages the first player if that is someone else; he follows
the first player token as it passes; and he never waits in the staging area.
Forced: When Gollum is defeated, flip him to Sméagol and return him to his owner's control,
exhausted. He is flipped as soon as he is defeated, once the attack that defeated him has
recorded it, so what answers an enemy being destroyed by an attack still sees it.
-}
instance RunMessage GollumASitE where
  runMessage msg e@(GollumASitE attrs) = case msg of
    CheckWindows ws | notNull [() | Window After (EntersPlay t) <- ws, t == toTarget attrs] -> do
      lead <- getLead
      when (attrs.placement /= EngagedWith lead) $ engageEnemy lead attrs.id
      GollumASitE <$> runMessage msg attrs
    CheckWindows ws | notNull [() | Window After (EnemyDefeated eid) <- ws, eid == attrs.id] -> do
      flipOver attrs
      GollumASitE <$> runMessage msg attrs
    SetFirstPlayer pid | attrs.placement /= EngagedWith pid -> do
      engageEnemy pid attrs.id
      GollumASitE <$> runMessage msg attrs
    PlaceEnemy eid StagingArea | eid == attrs.id -> do
      lead <- getLead
      engageEnemy lead attrs.id
      pure e
    _ -> GollumASitE <$> runMessage msg attrs
