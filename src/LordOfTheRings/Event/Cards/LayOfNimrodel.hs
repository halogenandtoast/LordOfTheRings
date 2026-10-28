module LordOfTheRings.Event.Cards.LayOfNimrodel (layOfNimrodel) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Spirit))

newtype LayOfNimrodel = LayOfNimrodel EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Choose a spirit hero. Until the end of the phase, that hero gets +1 willpower
-- for each resource in its resource pool.
layOfNimrodel :: EventCard LayOfNimrodel
layOfNimrodel = eventWith LayOfNimrodel Cards.layOfNimrodel (criteriaL .~ exists (HeroWithSphere Spirit <> HeroWithResources))

instance RunMessage LayOfNimrodel where
  runMessage msg e@(LayOfNimrodel attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      heroes <- traverse getHero =<< select (HeroWithSphere Spirit <> HeroWithResources)
      chooseOrRunOneM pid do
        questionLabeled "layOfNimrodel.chooseHero"
        causedBy attrs
        -- the pool is counted as it resolves (FAQ 1.55)
        targets heroes \h -> phaseModifier attrs h (WillpowerModifier h.resources)
      pure e
    _ -> LayOfNimrodel <$> runMessage msg attrs
