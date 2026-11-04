module LordOfTheRings.Treachery.Cards.OverseersWhip (overseersWhip) where

import LordOfTheRings.Helpers.Staging (getEngagementCost)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Trait (Trait (Easterling, Weapon))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype OverseersWhip = OverseersWhip TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, Entity)

overseersWhip :: TreacheryCard OverseersWhip
overseersWhip = treachery OverseersWhip Cards.overseersWhip

-- | While attached to an enemy, counts as a Weapon attachment with the text: "Forced: When
-- attached enemy attacks, deal 1 damage to an exhausted character you control."
instance HasModifiersFor OverseersWhip where
  getModifiersFor (OverseersWhip a) = case a.placement of
    AttachedTo (EnemyTarget _) -> modifySelf a [GainTrait Weapon]
    _ -> pure ()

instance HasAbilities OverseersWhip where
  getAbilities (OverseersWhip a) = case a.placement of
    AttachedTo (EnemyTarget eid) -> [mkAbility a 1 $ forced $ EnemyAttacksWindow When eid]
    _ -> []

{- | Surge.
When Revealed: Attach to the Easterling enemy with the lowest engagement cost: one with an
Overseer's Whip already attached too, as the card does not exclude it.
-}
instance RunMessage OverseersWhip where
  runMessage msg t@(OverseersWhip attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      enemies <- select (EnemyWithTrait Easterling)
      void $ attachToEnemyAmong attrs attrs.treacheryId enemies Lowest getEngagementCost (const $ pure ())
      pure t
    -- "you": the player the enemy attacks
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [p | Window _ (EnemyAttacking _ p) <- ws] \pid -> do
        exhausted <- exhaustedCharactersOf pid
        chooseOrRunOneM pid do
          questionLabeled "overseersWhip.damage"
          causedBy attrs
          targets exhausted \c -> dealDamage attrs c 1
      pure t
    _ -> OverseersWhip <$> runMessage msg attrs
