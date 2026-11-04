module LordOfTheRings.Event.Cards.Determination (determination) where

import LordOfTheRings.Campaign (CampaignLogKey (Determined), recorded)
import LordOfTheRings.Card (cdUnique, toCardCode, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Entities (entitiesAttachments)
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Placement (Placement (AttachedTo))

newtype Determination = Determination EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | A boon of the Angmar Awakened campaign, from The Wastes of Eriador.
determination :: EventCard Determination
determination = event Determination Cards.determination

{- | Action: Attach Determination to a unique character. Then ready each character with a
copy of Determination attached, as well as each character who is "determined" in the
Campaign Log. (Counts as a Condition attachment with the text: "Forced: If you win this
scenario, mark in the Campaign Log that attached character is 'determined'.") Attached,
it is "LordOfTheRings.Attachment.Cards.DeterminationAttachment".
-}
instance RunMessage Determination where
  runMessage msg e@(Determination attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      heroes <- traverse getHero =<< select AnyHero
      allies <- filter (cdUnique . toCardDef) <$> (traverse getAlly =<< select AnyAlly)
      let characters = [(toTarget h.id, toCardCode h) | h <- heroes] <> [(toTarget a.id, toCardCode a) | a <- allies]
      determined <- getsGame (foldMap (recorded Determined) . (.campaign) . toAttrs . (.scenario))
      copies <- getsGame (filter ((== toCardCode Cards.determination) . toCardCode) . toList . entitiesAttachments . gameEntities)
      let attached = [h | a <- copies, AttachedTo h <- [a.placement]]
      chooseOrRunOneM pid do
        questionLabeled "determination.attach"
        causedBy attrs
        for_ characters \(t, _) -> targeting t do
          -- it stays in play, attached, rather than going to the discard pile
          removeFromDiscard pid card.id
          attachOutOfPlay pid card t
          for_ [t' | (t', c) <- characters, t' `elem` (t : attached) || c `elem` determined] ready
      pure e
    _ -> Determination <$> runMessage msg attrs
