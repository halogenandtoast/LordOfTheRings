module LordOfTheRings.Attachment.Cards.KingOfDale (kingOfDale) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (CardType (AllyType), cdName, toCardDef)
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Trait (Trait (Dale, Esgaroth))

newtype KingOfDale = KingOfDale AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Dale or Esgaroth hero.
kingOfDale :: AttachmentCard KingOfDale
kingOfDale = attachment KingOfDale Cards.kingOfDale & attachTo (AttachToHero $ HeroMatchAny [HeroWithTrait Dale, HeroWithTrait Esgaroth])

-- | Planning Action: Exhaust King of Dale to reduce the cost of the next Dale ally you play
-- this phase by 1 for each different player attachment on attached hero. That ally does
-- not require a resource match.
instance HasAbilities KingOfDale where
  getAbilities (KingOfDale a) = [restricted a 1 (DuringPhase PlanningPhase) $ ActionAbility (exhaustCost a)]

instance RunMessage KingOfDale where
  runMessage msg s@(KingOfDale a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      case a.placement of
        AttachedTo host -> do
          -- different by title, King of Dale among them
          n <- length . nub . map (cdName . toCardDef) <$> playerAttachmentsOn host
          let daleAlly = CardWithType AllyType <> CardWithTrait Dale
          phaseModifiers a (PlayerTarget pid) [LowerNextCostOf daleAlly n, NextIgnoresResourceMatch daleAlly]
        _ -> pure ()
      pure s
    _ -> KingOfDale <$> runMessage msg a
