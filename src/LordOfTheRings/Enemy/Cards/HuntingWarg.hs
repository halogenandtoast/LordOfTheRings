module LordOfTheRings.Enemy.Cards.HuntingWarg (huntingWarg) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Phase (Phase (RefreshPhase))

newtype HuntingWarg = HuntingWarg EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

huntingWarg :: EnemyCard HuntingWarg
huntingWarg = enemy HuntingWarg Cards.huntingWarg

instance HasAbilities HuntingWarg where
  getAbilities (HuntingWarg a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After RefreshPhase]

-- | Forced: At the beginning of the refresh phase, if Hunting Warg entered play this round,
-- search the encounter deck and discard pile for 1 copy of Hunters from Mordor and add it
-- to the staging area, if able. Shuffle the encounter deck.
instance RunMessage HuntingWarg where
  runMessage msg e@(HuntingWarg attrs) = case msg of
    -- it marks the round it came into play
    CheckWindows ws | any (entered . windowType) ws -> do
      roundModifier attrs attrs EnteredPlayThisRound
      HuntingWarg <$> runMessage msg attrs
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      new <- elem EnteredPlayThisRound <$> getModifiers attrs
      lead <- getLead
      when new $ searchEncounterDeckAndDiscard lead attrs (cardIs Cards.huntersFromMordor)
      pure e
    _ -> HuntingWarg <$> runMessage msg attrs
   where
    entered = \case
      EntersStagingArea t -> isTarget attrs t
      EntersPlay t -> isTarget attrs t
      _ -> False
