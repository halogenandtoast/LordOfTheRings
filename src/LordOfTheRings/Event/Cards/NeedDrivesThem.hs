module LordOfTheRings.Event.Cards.NeedDrivesThem (needDrivesThem) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype NeedDrivesThem = NeedDrivesThem EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Each player whose threat is 40 or higher readies all characters he controls.
needDrivesThem :: EventCard NeedDrivesThem
needDrivesThem = event NeedDrivesThem Cards.needDrivesThem

instance RunMessage NeedDrivesThem where
  runMessage msg e@(NeedDrivesThem attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      eachPlayer \p -> do
        threat <- (.threat) <$> getPlayer p
        when (threat >= 40) do
          let you = PlayerWithId p
          traverse_ ready =<< selectCharacters (HeroControlledBy you <> ExhaustedHero) (AllyControlledBy you <> ExhaustedAlly)
      pure e
    _ -> NeedDrivesThem <$> runMessage msg attrs
