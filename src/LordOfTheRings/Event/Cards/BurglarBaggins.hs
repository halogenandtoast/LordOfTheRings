module LordOfTheRings.Event.Cards.BurglarBaggins (burglarBaggins) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype BurglarBaggins = BurglarBaggins EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, Entity)

-- | Action: Bilbo Baggins gets +2 [willpower], +2 [attack], and +2 [defense] until the end
-- of the phase.
burglarBaggins :: EventCard BurglarBaggins
burglarBaggins =
  eventWith BurglarBaggins Cards.burglarBaggins \a -> a & criteriaL .~ exists bilbo

bilbo :: HeroMatcher
bilbo = HeroWithTitle "Bilbo Baggins"

-- | (You may spend a [baggins] resource from Bilbo Baggins' resource pool to play this card
-- even if you do not control Bilbo Baggins.)
instance HasModifiersFor BurglarBaggins where
  getModifiersFor (BurglarBaggins a) = modifySelf a [PayableFromHeroes bilbo]

instance RunMessage BurglarBaggins where
  runMessage msg e@(BurglarBaggins attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      heroes <- select bilbo
      for_ heroes \h -> phaseModifiers attrs h [WillpowerModifier 2, AttackModifier 2, DefenseModifier 2]
      pure e
    _ -> BurglarBaggins <$> runMessage msg attrs
