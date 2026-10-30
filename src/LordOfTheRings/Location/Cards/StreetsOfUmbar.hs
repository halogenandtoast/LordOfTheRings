module LordOfTheRings.Location.Cards.StreetsOfUmbar (streetsOfUmbar) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.Types (EnemyAttrs (enemyTokens))
import LordOfTheRings.Helpers.Window (travelableLocations)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Raider))

newtype StreetsOfUmbar = StreetsOfUmbar LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

streetsOfUmbar :: LocationCard StreetsOfUmbar
streetsOfUmbar = location StreetsOfUmbar Cards.streetsOfUmbar

{- | When faced with the option to travel, if Streets of Umbar is in the staging area, the
players must either travel to a location named Streets of Umbar or add 2 resources to a
Raider enemy in play: asked once the players' travel actions are over, before the first
player may travel, by the first copy in the staging area. Having travelled, there is no
travel left to choose.
Shadow: If attacking enemy is a Raider, deal it an additional shadow card for each resource
token on it, to a maximum of 3 additional shadow cards.
-}
instance RunMessage StreetsOfUmbar where
  runMessage msg l@(StreetsOfUmbar a) = case msg of
    TravelActions -> do
      staged <- select (LocationInStagingArea <> LocationWithCardCode (toCardCode a))
      travelable <- travelableLocations
      when (minimumMay staged == Just a.id && notNull travelable) do
        lead <- getLead
        raiders <- select (EnemyWithTrait Raider)
        chooseOneM lead do
          questionLabeled "streetsOfUmbar.choose"
          causedBy a
          targets (filter (`elem` travelable) staged) (push . TravelTo)
          unless (null raiders) $ labeled "streetsOfUmbar.raider" $ chooseOrRunOneM lead do
            questionLabeled "streetsOfUmbar.chooseRaider"
            causedBy a
            targets raiders \r -> placeResources a r 2
      StreetsOfUmbar <$> runMessage msg a
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      raider <- selectAny (EnemyWithId eid <> EnemyWithTrait Raider)
      n <- countTokens Resource . enemyTokens . toAttrs <$> getEnemy eid
      when raider $ replicateM_ (min 3 n) (dealShadowCard eid)
      pure l
    _ -> StreetsOfUmbar <$> runMessage msg a
