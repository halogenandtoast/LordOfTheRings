module LordOfTheRings.Location.Cards.Fanuidhol (fanuidhol) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype Fanuidhol = Fanuidhol LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

fanuidhol :: LocationCard Fanuidhol
fanuidhol = location Fanuidhol Cards.fanuidhol

-- | While Fanuidhol is the active location, heroes must spend 1 resource from their
-- resource pool to count their [Willpower] during the quest phase.
--
-- Asked as each hero commits: a hero that does not pay does not count its willpower this
-- phase.
instance RunMessage Fanuidhol where
  runMessage msg (Fanuidhol a) = case msg of
    CommitToQuest hero@(HeroTarget hid) | a.placement == ActiveLocation -> do
      h <- getHero hid
      if h.resources > 0
        then chooseOneM h.owner do
          questionLabeled "fanuidhol.pay"
          causedBy a
          aimedAt [hero]
          labeled "fanuidhol.spend" $ removeTokens a hero Resource 1
          labeled "fanuidhol.decline" $ phaseModifier a hero DoesNotCountWillpower
        else phaseModifier a hero DoesNotCountWillpower
      Fanuidhol <$> runMessage msg a
    _ -> Fanuidhol <$> runMessage msg a
