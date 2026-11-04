module LordOfTheRings.Event.Cards.Bartering (bartering) where

import LordOfTheRings.Attachment (AttachmentAttrs (attachmentExhausted))
import LordOfTheRings.Card (CardCost (StaticCost), cdCost, toCardDef)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (PlanningPhase))

newtype Bartering = Bartering EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

bartering :: EventCard Bartering
bartering =
  eventWith Bartering Cards.bartering \a ->
    let you = PlayerWithId a.owner
     in a
          & criteriaL
          .~ ( DuringPhase PlanningPhase
                <> AnyCriteria
                  [ exists (HeroControlledBy you <> HeroWithReadyPlayerAttachment)
                  , exists (AllyControlledBy you <> AllyWithReadyPlayerAttachment)
                  ]
             )

-- | Planning Action: Choose a ready player attachment on a character you control. Return
-- that attachment to its owner's hand to reduce the cost of the next attachment played
-- this phase by X, where X is the chosen attachment's printed cost.
instance RunMessage Bartering where
  runMessage msg e@(Bartering attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      attached <- concat <$> (traverse playerAttachmentsOn =<< charactersControlledBy pid)
      chooseOrRunOneM pid do
        questionLabeled "bartering.attachment"
        causedBy attrs
        targets (filter (not . attachmentExhausted . toAttrs) attached) \att -> do
          returnToHand att
          case cdCost (toCardDef att) of
            Just (StaticCost x) | x > 0 -> phaseModifier attrs GameTarget (LowerNextCostOf (CardWithType AttachmentType) x)
            _ -> pure ()
      pure e
    _ -> Bartering <$> runMessage msg attrs
