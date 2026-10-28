module LordOfTheRings.Event.Cards.StrengthOfArms (strengthOfArms) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Leadership))

newtype StrengthOfArms = StrengthOfArms EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Play only if each hero you control has a printed leadership resource icon.
-- Action: Ready each ally in play.
strengthOfArms :: EventCard StrengthOfArms
strengthOfArms =
  eventWith StrengthOfArms Cards.strengthOfArms \a ->
    a & criteriaL .~ eachHeroHasPrintedIcon a.owner Leadership <> exists ExhaustedAlly

instance RunMessage StrengthOfArms where
  runMessage msg e@(StrengthOfArms attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      traverse_ ready =<< select AnyAlly
      pure e
    _ -> StrengthOfArms <$> runMessage msg attrs
