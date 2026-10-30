module LordOfTheRings.Location.Cards.QuarterDeck (quarterDeck) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype QuarterDeck = QuarterDeck LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

quarterDeck :: LocationCard QuarterDeck
quarterDeck = location QuarterDeck Cards.quarterDeck

-- | While Quarter Deck is the active location, deal each enemy an additional shadow card at
-- the beginning of the combat phase: each engaged with a player, as the others are dealt.
-- Shadow: Discard an attachment you control.
instance RunMessage QuarterDeck where
  runMessage msg l@(QuarterDeck attrs) = case msg of
    DealShadowCards | attrs.placement == ActiveLocation -> do
      enemies <- select (EnemyEngagedWith AnyPlayer <> not_ EnemyInStagingArea)
      for_ enemies dealShadowCard
      QuarterDeck <$> runMessage msg attrs
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      attachments <- getAttachmentsControlledBy pid
      unless (null attachments) $ chooseAndDiscard attrs pid attachments
      pure l
    _ -> QuarterDeck <$> runMessage msg attrs
