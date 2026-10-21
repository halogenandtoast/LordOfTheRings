module LordOfTheRings.Ally.Cards.KeenEyedTook (keenEyedTook) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype KeenEyedTook = KeenEyedTook AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

keenEyedTook :: AllyCard KeenEyedTook
keenEyedTook = ally KeenEyedTook Cards.keenEyedTook

-- | Response: After Keen-eyed Took enters play, reveal the top card of each player's deck.
-- Action: Return Keen-eyed Took to your hand to discard the top card of each player's
-- deck.
instance HasAbilities KeenEyedTook where
  getAbilities (KeenEyedTook a) =
    [ mkAbility a 1 $ response $ entersPlay a
    , mkAbility a 2 $ ActionAbility Free
    ]

instance RunMessage KeenEyedTook where
  runMessage msg s@(KeenEyedTook a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      tops <- concatMap (take 1 . (.deck)) <$> (traverse getPlayer =<< getRemainingPlayers)
      unless (null tops) $ chooseOneM pid do
        questionLabeled "keenEyedTook.reveal"
        causedBy a
        showing tops
        labeled "continue" $ pure ()
      pure s
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      returnToHand a
      eachPlayer \p -> discardTopOfDeck p 1
      pure s
    _ -> KeenEyedTook <$> runMessage msg a
