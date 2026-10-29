module LordOfTheRings.Ally.Cards.BlueMountainTrader (blueMountainTrader) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype BlueMountainTrader = BlueMountainTrader AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

blueMountainTrader :: AllyCard BlueMountainTrader
blueMountainTrader = ally BlueMountainTrader Cards.blueMountainTrader

-- | Action: Choose another player. That player gains control of Blue Mountain Trader. Then,
-- that player moves 1 resource from the resource pool of a hero he controls to the
-- resource pool of a hero you control, or Blue Mountain Trader is discarded. (Limit once
-- per round.)
instance HasAbilities BlueMountainTrader where
  getAbilities (BlueMountainTrader a) =
    [limited (GroupLimit PerRound 1) $ restricted a 1 (PlayerExists $ NotPlayer ActivePlayer) $ ActionAbility Free]

instance RunMessage BlueMountainTrader where
  runMessage msg s@(BlueMountainTrader a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      others <- filter (/= pid) <$> getRemainingPlayers
      chooseOrRunOneM pid do
        questionLabeled "choosePlayer"
        causedBy a
        targets others \p -> do
          takeControl a.id p
          theirs <- select (HeroControlledBy (PlayerWithId p) <> HeroWithResources)
          yours <- select (HeroControlledBy (PlayerWithId pid))
          chooseOneM p do
            questionLabeled "blueMountainTrader.move"
            causedBy a
            targets theirs \h -> chooseOrRunOneM p do
              questionLabeled "moveResourceTo"
              causedBy a
              targets yours \h' -> do
                removeTokens a h Resource 1
                placeResources a h' 1
            labeled "blueMountainTrader.discard" $ discard a
      pure s
    _ -> BlueMountainTrader <$> runMessage msg a
