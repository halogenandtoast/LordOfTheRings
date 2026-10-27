module LordOfTheRings.Location.Cards.PerilousSwamp (perilousSwamp) where

import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype PerilousSwamp = PerilousSwamp LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

perilousSwamp :: LocationCard PerilousSwamp
perilousSwamp = location PerilousSwamp Cards.perilousSwamp

-- | No more than 1 progress token can be placed on Perilous Swamp each round. What
-- questing places beyond that is lost: the swamp is not explored, so none of it reaches
-- the quest.
instance HasModifiersFor PerilousSwamp where
  getModifiersFor (PerilousSwamp a) = modified a a [ProgressLimit 1]

-- | Shadow: Remove 1 progress token from the current quest.
instance RunMessage PerilousSwamp where
  runMessage msg l@(PerilousSwamp a) = case msg of
    PlaceTokens source (isTarget a -> True) Progress n | n > 0 -> do
      limit <- foldr min n . (\ms -> [k | ProgressLimit k <- ms]) <$> getModifiers a
      -- the round's token is placed
      roundModifier a a (ProgressLimit 0)
      if limit > 0
        then PerilousSwamp <$> runMessage (PlaceTokens source (toTarget a) Progress limit) a
        else pure l
    ResolveShadowEffect cid _ _ _ | cid == a.cardId -> do
      removeProgressFromQuest a 1
      pure l
    _ -> PerilousSwamp <$> runMessage msg a
