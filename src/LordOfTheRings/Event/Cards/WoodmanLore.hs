module LordOfTheRings.Event.Cards.WoodmanLore (woodmanLore) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Message.Lifted.UpTo
import LordOfTheRings.Trait (Trait (Woodman))

newtype WoodmanLore = WoodmanLore EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

woodmanLore :: EventCard WoodmanLore
woodmanLore = event WoodmanLore Cards.woodmanLore

-- | Action: Ready up to X Woodman characters you control, where X is the number of
-- locations with player card attachments.
instance RunMessage WoodmanLore where
  runMessage msg e@(WoodmanLore attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      let you = PlayerWithId pid
      x <- length <$> select LocationWithPlayerCardAttachment
      woodmen <-
        selectCharacters
          (HeroControlledBy you <> HeroWithTrait Woodman <> ExhaustedHero)
          (AllyControlledBy you <> AllyWithTrait Woodman <> ExhaustedAlly)
      chooseUpToM pid "woodmanLore.ready" (toTarget attrs) [] x [UpToTarget c (ready c) | c <- woodmen]
      pure e
    _ -> WoodmanLore <$> runMessage msg attrs
