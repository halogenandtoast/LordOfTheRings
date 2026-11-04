module LordOfTheRings.Event.Cards.TakeNoNotice (takeNoNotice) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Hobbit, Ranger))

newtype TakeNoNotice = TakeNoNotice EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, Entity)

takeNoNotice :: EventCard TakeNoNotice
takeNoNotice = event TakeNoNotice Cards.takeNoNotice

-- | Lower the cost to play Take No Notice by 1 for each Hobbit or Ranger hero you control.
instance HasModifiersFor TakeNoNotice where
  getModifiersFor (TakeNoNotice a) = do
    n <- length <$> select (HeroControlledBy (PlayerWithId a.owner) <> HeroMatchAny [HeroWithTrait Hobbit, HeroWithTrait Ranger])
    when (n > 0) $ modifySelf a [CostModifier (negate n)]

-- | Action: Add 5 to each enemy's engagement cost until the end of the round.
instance RunMessage TakeNoNotice where
  runMessage msg e@(TakeNoNotice attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      enemies <- select AnyEnemy
      for_ enemies \eid -> roundModifier attrs eid (EngagementCostModifier 5)
      pure e
    _ -> TakeNoNotice <$> runMessage msg attrs
