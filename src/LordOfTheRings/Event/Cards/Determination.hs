module LordOfTheRings.Event.Cards.Determination (determination) where

import LordOfTheRings.Campaign (CampaignLogKey (Determined), CampaignLogUpdate (Record), recorded)
import LordOfTheRings.Card (cdUnique, toCardCode, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype Determination = Determination EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | A boon of the Angmar Awakened campaign, from The Wastes of Eriador.
determination :: EventCard Determination
determination = event Determination Cards.determination

{- | Action: Attach Determination to a unique character. Then ready each character with a
copy of Determination attached, as well as each character who is "determined" in the
Campaign Log. (Counts as a Condition attachment with the text: "Forced: If you win this
scenario, mark in the Campaign Log that attached character is 'determined'.")

-}
instance RunMessage Determination where
  runMessage msg e@(Determination attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      heroes <- traverse getHero =<< select AnyHero
      allies <- filter (cdUnique . toCardDef) <$> (traverse getAlly =<< select AnyAlly)
      let characters = [(toTarget h.id, toCardCode h) | h <- heroes] <> [(toTarget a.id, toCardCode a) | a <- allies]
      determined <- getsGame (foldMap (recorded Determined) . (.campaign) . toAttrs . (.scenario))
      chooseOrRunOneM pid do
        questionLabeled "determination.attach"
        causedBy attrs
        for_ characters \(t, code) -> targeting t do
          let marked = ordNub (determined <> [code])
          updateCampaignLog (Record Determined marked)
          for_ [t' | (t', c) <- characters, c `elem` marked] ready
      pure e
    _ -> Determination <$> runMessage msg attrs
