module LordOfTheRings.Ally.Cards.LongLakeTrader (longLakeTrader) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (toCardCode, toCardDef)
import LordOfTheRings.Helpers.Attachment (cardHosts)
import LordOfTheRings.Placement (Placement (AttachedTo), placementController)
import LordOfTheRings.Trait (Trait (Item))

newtype LongLakeTrader = LongLakeTrader AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

longLakeTrader :: AllyCard LongLakeTrader
longLakeTrader = ally LongLakeTrader Cards.longLakeTrader

-- | Action: Exhaust Long Lake Trader to move an Item attachment from a character you
-- control to another eligible character.
instance HasAbilities LongLakeTrader where
  getAbilities (LongLakeTrader a) =
    [ restricted a 1 (AnyCriteria [exists (HeroControlledBy you <> HeroWithAttached Item), exists (AllyControlledBy you <> AllyWithAttached Item)])
        $ ActionAbility (exhaustCost a)
    | Just pid <- [placementController a.placement]
    , let you = PlayerWithId pid
    ]

instance RunMessage LongLakeTrader where
  runMessage msg s@(LongLakeTrader a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      attached <- concat <$> (traverse playerAttachmentsOn =<< charactersControlledBy pid)
      items <- filterM (\att -> hasTrait Item (toTarget att) (toCardDef att)) attached
      moves <- for items \att -> do
        hosts <- cardHosts (toAttrs att).controller (toCardCode att)
        pure (att, [h | h <- hosts, AttachedTo h /= att.placement])
      chooseOneM pid do
        questionLabeled "longLakeTrader.attachment"
        causedBy a
        for_ [m | m@(_, _ : _) <- moves] \(att, hosts) -> targeting att do
          chooseOneM pid do
            questionLabeled "longLakeTrader.host"
            causedBy a
            targets hosts $ moveAttachment (toId att)
      pure s
    _ -> LongLakeTrader <$> runMessage msg a
