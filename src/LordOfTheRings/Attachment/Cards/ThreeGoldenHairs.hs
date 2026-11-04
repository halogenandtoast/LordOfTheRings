module LordOfTheRings.Attachment.Cards.ThreeGoldenHairs (threeGoldenHairs) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Gift (spendBoon)
import LordOfTheRings.Attachment.Import.Lifted

newtype ThreeGoldenHairs = ThreeGoldenHairs AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A Gift boon of The Road Darkens. Setup: Attach to a hero in play: as the game is set
-- up, from its owner's deck (its card def's setup attachment).
threeGoldenHairs :: AttachmentCard ThreeGoldenHairs
threeGoldenHairs = attachment ThreeGoldenHairs Cards.threeGoldenHairs & attachTo (AttachToHero AnyHero)

-- | Action: Add Three Golden Hairs to the victory display and remove it from the campaign pool,
-- to lower each player's threat by 3. Then, each player draws 3 cards.
instance HasAbilities ThreeGoldenHairs where
  getAbilities (ThreeGoldenHairs a) = [mkAbility a 1 $ ActionAbility Free]

instance RunMessage ThreeGoldenHairs where
  runMessage msg s@(ThreeGoldenHairs a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      spendBoon a
      eachPlayer \p -> lowerThreatBy a p 3
      eachPlayer \p -> drawCards a p 3
      pure s
    _ -> ThreeGoldenHairs <$> runMessage msg a
