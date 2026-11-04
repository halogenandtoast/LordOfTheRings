module LordOfTheRings.Treachery.Cards.RecurveBow (recurveBow) where

import LordOfTheRings.Helpers.Staging (getEngagementCost)
import LordOfTheRings.Keyword (Keyword (Archery))
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Trait (Trait (Easterling, Weapon))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype RecurveBow = RecurveBow TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

recurveBow :: TreacheryCard RecurveBow
recurveBow = treachery RecurveBow Cards.recurveBow

-- | While attached to an enemy, counts as a Weapon attachment with the text: "Attached
-- enemy gains Archery 2."
instance HasModifiersFor RecurveBow where
  getModifiersFor (RecurveBow a) = case a.placement of
    AttachedTo host@(EnemyTarget _) -> do
      modifySelf a [GainTrait Weapon]
      modified a host [GainKeyword (Archery 2)]
    _ -> pure ()

-- | Surge.
-- When Revealed: Attach to the highest engagement cost Easterling enemy without a Recurve
-- Bow.
instance RunMessage RecurveBow where
  runMessage msg t@(RecurveBow attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      void $ attachToEnemyWith attrs attrs.treacheryId attrs.treacheryCardCode (EnemyWithTrait Easterling) Highest getEngagementCost (const $ pure ())
      pure t
    _ -> RecurveBow <$> runMessage msg attrs
