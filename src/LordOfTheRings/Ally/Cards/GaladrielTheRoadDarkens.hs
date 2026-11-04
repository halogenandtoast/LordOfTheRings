module LordOfTheRings.Ally.Cards.GaladrielTheRoadDarkens (galadrielTheRoadDarkens) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (Card (PlayerCard), CardType (AttachmentType))

newtype GaladrielTheRoadDarkens = GaladrielTheRoadDarkens AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

galadrielTheRoadDarkens :: AllyCard GaladrielTheRoadDarkens
galadrielTheRoadDarkens = ally GaladrielTheRoadDarkens Cards.galadrielTheRoadDarkens

-- | At the end of the round, discard Galadriel from play.
instance HasModifiersFor GaladrielTheRoadDarkens where
  getModifiersFor (GaladrielTheRoadDarkens a) = modifySelf a [DiscardAtEndOfRound]

-- | Response: After you play Galadriel from your hand, search the top 5 cards of your deck
-- for an attachment of cost 3 or less and put it into play. Put the remaining cards back
-- in any order.
instance HasAbilities GaladrielTheRoadDarkens where
  getAbilities (GaladrielTheRoadDarkens a) = [mkAbility a 1 $ response $ PlayedFromHandWindow After (CardIdTarget a.allyCardId)]

instance RunMessage GaladrielTheRoadDarkens where
  runMessage msg s@(GaladrielTheRoadDarkens a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      top <- take 5 . (.deck) <$> getPlayer pid
      let matcher = CardWithType AttachmentType <> NotCard (CardWithPrintedCostAtLeast 4)
      found <- filterM (passesUnique . PlayerCard) [pc | PlayerCard pc <- top, cardMatches matcher pc]
      chooseOneM pid do
        questionLabeled "galadrielTheRoadDarkens.choose"
        causedBy a
        showing top
        for_ found \pc -> targeting pc do
          removeFromDeck pid pc.id
          putAttachmentIntoPlay pid pc
          returnToTopOfDeckInAnyOrder pid a [c | c <- top, c.id /= pc.id]
        labeled "galadrielTheRoadDarkens.none" $ returnToTopOfDeckInAnyOrder pid a top
      pure s
    _ -> GaladrielTheRoadDarkens <$> runMessage msg a
