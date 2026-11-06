module LordOfTheRings.Treachery.Cards.LostInTheDark (lostInTheDark) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.IntoThePit.Helpers (readyCaveTorch, caveTorchController)

newtype LostInTheDark = LostInTheDark TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

lostInTheDark :: TreacheryCard LostInTheDark
lostInTheDark = treachery LostInTheDark Cards.lostInTheDark

instance RunMessage LostInTheDark where
  runMessage msg t@(LostInTheDark a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      torch <- readyCaveTorch
      pid <- maybe getLead pure =<< caveTorchController
      chooseOrRunOneM pid do
        questionLabeled "lostInTheDark.choose"
        causedBy a
        for_ torch \(oid, _) -> labeled "lostInTheDark.exhaust" $ exhaust oid
        labeled "lostInTheDark.noProgress" $
          traverse_ (\q -> phaseModifier a q (ProgressLimit 0)) =<< getCurrentQuest
      pure t
    ResolveShadowEffect cid _ _ defenders | cid == a.cardId -> do
      for_ defenders \c -> dealDamage a c 1
      pure t
    _ -> LostInTheDark <$> runMessage msg a
