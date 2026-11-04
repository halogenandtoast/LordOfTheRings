module LordOfTheRings.Quest.Cards.MysteriousOmens (mysteriousOmens) where

import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign (CampaignLogUpdate (..), PoolCard (..))
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype MysteriousOmens = MysteriousOmens QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest of the Angmar Awakened campaign, a burden shuffled into the
-- encounter deck from The Weather Hills on until it is defeated. Surge, on its card def.
mysteriousOmens :: QuestCard MysteriousOmens
mysteriousOmens = quest MysteriousOmens Cards.mysteriousOmens

{- | Forced: After Mysterious Omens becomes the current quest, each player discards 1 random
card from their hand.
Forced: When Mysterious Omens is defeated, remove it from the Campaign Pool. Then, each
player may choose 1 of the following boons and attach it to a hero they control: Cunning,
Hardy, Resolute, Ruthless, or Stalwart. Each boon chosen joins the Campaign Pool on that
hero, as a Permanent card.
The players choose in turn order, each boon taken once (the box has 1 of each).
-}
instance RunMessage MysteriousOmens where
  runMessage msg (MysteriousOmens attrs) = case msg of
    SetCurrentQuest (isTarget attrs -> True) -> do
      eachPlayer \p -> discardRandomFromHand p 1
      MysteriousOmens <$> runMessage msg attrs
    CompleteQuest qid | qid == attrs.id -> do
      updateCampaignLog $ RemoveFromPool $ toCardCode Cards.mysteriousOmens
      eachPlayerMayTakeOneOf
        "mysteriousOmens.chooseBoon"
        (Just $ toTarget attrs)
        [Attachments.cunning, Attachments.hardy, Attachments.resolute, Attachments.ruthless, Attachments.stalwart]
        \p boon -> do
          heroes <- traverse getHero =<< select (HeroControlledBy $ PlayerWithId p)
          chooseOrRunOneM p do
            questionLabeled "campaign.attachTo"
            targets heroes \h -> do
              attachOutOfPlay p boon h.id
              updateCampaignLog $ AddToPool $ PoolCard (toCardCode boon) Nothing (Just $ toCardCode h)
      MysteriousOmens <$> runMessage msg attrs
    _ -> MysteriousOmens <$> runMessage msg attrs
