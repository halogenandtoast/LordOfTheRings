module LordOfTheRings.Attachment.Cards.ExpertTreasureHunter (expertTreasureHunter) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (Card, CardType (..), cdCardType, isTreasure, toCardDef)

newtype ExpertTreasureHunter = ExpertTreasureHunter AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a hero. Limit 1 per hero, on its card def (FAQ 1.9 errata).
expertTreasureHunter :: AttachmentCard ExpertTreasureHunter
expertTreasureHunter = attachment ExpertTreasureHunter Cards.expertTreasureHunter

-- | Response: After attached hero quests successfully, name a card type and discard the top
-- card of your deck. If the discarded card is the named type, take it into your hand.
instance HasAbilities ExpertTreasureHunter where
  getAbilities (ExpertTreasureHunter a) =
    [ restricted a 1 (exists $ HeroWithId hid <> CommittedHero) $ response $ QuestedSuccessfullyWindow After
    | AttachedTo (HeroTarget hid) <- [a.placement]
    ]

{- | The types a player card can be, as the Over Hill and Under Hill rules insert names them
for a riddle: ally, attachment, event or treasure. A treasure is not an attachment by name,
though it reads "Attach to".
-}
instance RunMessage ExpertTreasureHunter where
  runMessage msg s@(ExpertTreasureHunter a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      top <- headMay . (.deck) <$> getPlayer pid
      let named matches = do
            discardTopOfDeck pid 1
            for_ top \c -> when (matches c) $ returnFromDiscardToHand pid c.id
      chooseOneM pid do
        questionLabeled "expertTreasureHunter.name"
        causedBy a
        labeled "expertTreasureHunter.ally" $ named (ofType AllyType)
        labeled "expertTreasureHunter.attachment" $ named (\c -> ofType AttachmentType c && not (isTreasure c))
        labeled "expertTreasureHunter.event" $ named (ofType EventType)
        labeled "expertTreasureHunter.treasure" $ named isTreasure
      pure s
    _ -> ExpertTreasureHunter <$> runMessage msg a

ofType :: CardType -> Card -> Bool
ofType t = (== t) . cdCardType . toCardDef
