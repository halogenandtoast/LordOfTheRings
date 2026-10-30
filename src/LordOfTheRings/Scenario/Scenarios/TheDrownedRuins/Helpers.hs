-- | What the cards of The Drowned Ruins share: the active location under water.
module LordOfTheRings.Scenario.Scenarios.TheDrownedRuins.Helpers (
  underwaterActive,
  underwaterBan,
  mayFlipToGrottoSide,
) where

import LordOfTheRings.Card.CardType (CardType (AllyType, AttachmentType))
import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Underwater))

-- | Whether the active location is Underwater.
underwaterActive :: HasGame m => m Bool
underwaterActive = selectAny (IsActiveLocation <> LocationWithTrait Underwater)

-- | While an Underwater location is the active location, players cannot play attachments
-- or allies.
underwaterBan :: (HasModifiersM m, Sourceable source) => source -> m ()
underwaterBan source =
  whenM underwaterActive
    $ modified source GameTarget [CannotPlay (CardMatchAny [CardWithType AllyType, CardWithType AttachmentType])]

-- | "Then, you may flip it to its Grotto side": the first player decides, unless it cannot
-- be flipped now.
mayFlipToGrottoSide :: ReverseQueue m => LocationAttrs -> m ()
mayFlipToGrottoSide a = do
  barred <- elem CannotBeFlipped <$> getModifiers a
  lead <- getLead
  unless barred $ chooseOneM lead do
    questionLabeled "theDrownedRuins.mayFlip"
    causedBy a
    labeled "theDrownedRuins.flip" $ flipOver a
    labeled "theDrownedRuins.stay" $ pure ()
