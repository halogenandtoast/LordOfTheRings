module LordOfTheRings.Attachment.Cards.TheArkenstoneTreasure (theArkenstoneTreasure) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (CardType (AllyType), cdCost, costAsResources, toCardDef)

newtype TheArkenstoneTreasure = TheArkenstoneTreasure AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The treasure The Arkenstone of The Lonely Mountain. Attach to a hero.
theArkenstoneTreasure :: AttachmentCard TheArkenstoneTreasure
theArkenstoneTreasure = attachment TheArkenstoneTreasure Cards.theArkenstoneTreasure

-- | Action: Exhaust The Arkenstone and raise your threat by X to lower by X the cost of the
-- next ally you play this phase that matches the attached hero's sphere.
instance HasAbilities TheArkenstoneTreasure where
  getAbilities (TheArkenstoneTreasure a) = [mkAbility a 1 $ ActionAbility (exhaustCost a) | AttachedTo (HeroTarget _) <- [a.placement]]

{- | X is chosen from 1 to the cost of the costliest matching ally among the player's cards,
at least 1: lowering a cost below 0 lowers it no more.
-}
instance RunMessage TheArkenstoneTreasure where
  runMessage msg s@(TheArkenstoneTreasure a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      for_ [hid | AttachedTo (HeroTarget hid) <- [a.placement]] \hid -> do
        sphere <- (.sphere) <$> getHero hid
        let matching = CardWithType AllyType <> CardWithSphere sphere
        player <- getPlayer pid
        let cards = player.hand <> player.deck <> player.discard
            costliest = maximumMay [costAsResources c | card <- cards, cardMatches matching card, Just c <- [cdCost (toCardDef card)]]
        chooseAmountM pid "theArkenstone.chooseX" a 1 (max 1 $ fromMaybe 1 costliest) \x -> do
          raiseThreat pid x
          phaseModifier a (PlayerTarget pid) (LowerNextCostOf matching x)
      pure s
    _ -> TheArkenstoneTreasure <$> runMessage msg a
