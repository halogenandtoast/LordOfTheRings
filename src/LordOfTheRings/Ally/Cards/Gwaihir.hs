module LordOfTheRings.Ally.Cards.Gwaihir (gwaihir) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (Card (PlayerCard), CardType (AllyType))
import LordOfTheRings.Trait (Trait (Eagle))

newtype Gwaihir = Gwaihir AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

gwaihir :: AllyCard Gwaihir
gwaihir = ally Gwaihir Cards.gwaihir

-- | Cannot have restricted attachments.
instance HasModifiersFor Gwaihir where
  getModifiersFor (Gwaihir a) = modifySelf a [CannotHaveRestrictedAttachments]

eagleAlly :: CardMatcher
eagleAlly = CardWithType AllyType <> CardWithTrait Eagle

-- | Response: After Gwaihir enters play, search your discard pile for an Eagle ally and put
-- it into play under your control. At the end of the round, if that ally is still in play,
-- add it to your hand.
instance HasAbilities Gwaihir where
  getAbilities (Gwaihir a) = [restricted a 1 (CardInDiscard a.owner eagleAlly) $ response $ entersPlay a]

instance RunMessage Gwaihir where
  runMessage msg s@(Gwaihir a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      eagles <- filterM passesUnique . filter (cardMatches eagleAlly) . (.discard) =<< getPlayer a.owner
      chooseOrRunOneM pid do
        questionLabeled "gwaihir.chooseEagle"
        causedBy a
        targets [pc | PlayerCard pc <- eagles] \pc -> do
          removeFromDiscard a.owner pc.id
          roundModifier a (CardIdTarget pc.id) ReturnToHandAtEndOfRound
          putAllyIntoPlayUnder pid pc
      pure s
    _ -> Gwaihir <$> runMessage msg a
