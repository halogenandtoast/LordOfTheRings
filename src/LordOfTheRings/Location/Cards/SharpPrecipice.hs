module LordOfTheRings.Location.Cards.SharpPrecipice (sharpPrecipice) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gamePhase)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (TravelPhase))
import LordOfTheRings.Placement (Placement (StagingArea))

-- | A Grotto deck card, over Submerged Crawlway.
newtype SharpPrecipice = SharpPrecipice LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

sharpPrecipice :: LocationCard SharpPrecipice
sharpPrecipice = location SharpPrecipice Cards.sharpPrecipice

{- | Forced: When faced with the option to travel, if you do not choose to travel to a
location named Sharp Precipice, you must raise each player's threat by 3: the players
traveled elsewhere during the travel phase, or end it with no active location.
Forced: After you travel to Sharp Precipice, flip it to its Underwater side.
-}
instance HasAbilities SharpPrecipice where
  getAbilities (SharpPrecipice a) =
    mkAbility a 2 (forced $ TraveledWindow After a.id)
      : [ ability
        | a.placement == StagingArea
        , ability <- [mkAbility a 1 $ forced $ AnyTraveledWindow After, mkAbility a 3 $ forced $ PhaseEndsWindow When TravelPhase]
        ]

instance RunMessage SharpPrecipice where
  runMessage msg l@(SharpPrecipice a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      phase <- getsGame gamePhase
      elsewhere <- anyM (fmap ((/= toCardCode Cards.sharpPrecipice) . toCardCode) . getLocation) [lid | Window _ (Traveled lid) <- ws]
      when (phase == TravelPhase && elsewhere) $ eachPlayer \p -> raiseThreat p 3
      pure l
    UseCardAbility _ (isSource a -> True) 2 _ -> do
      flipOver a
      pure l
    UseCardAbility _ (isSource a -> True) 3 _ -> do
      whenM (selectNone IsActiveLocation) $ eachPlayer \p -> raiseThreat p 3
      pure l
    _ -> SharpPrecipice <$> runMessage msg a
