module LordOfTheRings.Treachery.Cards.HungryTroll (hungryTroll) where

import LordOfTheRings.Scenario.Scenarios.WeMustAwayEreBreakOfDay.Helpers (trollEnemy, unlessBagginsCancel)
import LordOfTheRings.Trait (Trait (Sack))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype HungryTroll = HungryTroll TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

hungryTroll :: TreacheryCard HungryTroll
hungryTroll = treachery HungryTroll Cards.hungryTroll

-- | When Revealed: The first player chooses a Troll enemy in the staging area and engages
-- that enemy. If there are no Troll enemies in the staging area, this card gains Surge.
-- Shadow: Deal 4 damage to each character with a Sack card attached. (The first player may
-- spend 2 Baggins resources to cancel this effect.)
instance RunMessage HungryTroll where
  runMessage msg t@(HungryTroll attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      trolls <- select (trollEnemy <> EnemyInStagingArea)
      if null trolls
        then revealEncounterCard
        else chooseOrRunOneM lead do
          questionLabeled "hungryTroll.engage"
          causedBy attrs
          targets trolls (engageEnemy lead)
      pure t
    ResolveShadowEffect cid _ _ _ | cid == attrs.cardId -> do
      unlessBagginsCancel attrs 2 do
        sacked <- selectCharacters (HeroWithAttached Sack) (AllyWithAttached Sack)
        for_ sacked \c -> dealDamage (CardIdSource cid) c 4
      pure t
    _ -> HungryTroll <$> runMessage msg attrs
