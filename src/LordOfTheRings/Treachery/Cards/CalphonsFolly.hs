module LordOfTheRings.Treachery.Cards.CalphonsFolly (calphonsFolly) where

import LordOfTheRings.Card (cdOtherSide, toCardCode, toCardDef)
import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype CalphonsFolly = CalphonsFolly TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

calphonsFolly :: TreacheryCard CalphonsFolly
calphonsFolly = treachery CalphonsFolly Cards.calphonsFolly

-- | The Uncharted locations whose other side has a Forced effect, each its first ability:
-- "after it is flipped over while active".
withForcedEffect :: [CardDef]
withForcedEffect =
  [ Locations.shrineToMorgoth
  , Locations.lushJungle
  , Locations.drownedGraves
  , Locations.shrineToMorgothNightmare
  , Locations.lushJungleNightmare
  , Locations.drownedGravesNightmare
  , Locations.desecratedGrounds
  , Locations.forsakenCity
  ]

{- | When Revealed: Choose an Uncharted location in the staging area and flip it over. If it
has a Forced effect, resolve it as if that location were active. If it does not have a
Forced effect, switch it with the active location, if able.
Shadow: Attacking enemy gets +X attack, where X is the active location's threat.
-}
instance RunMessage CalphonsFolly where
  runMessage msg t@(CalphonsFolly attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      uncharted <- select (LocationInStagingArea <> unchartedLocation)
      active <- select IsActiveLocation
      chooseOrRunOneM lead do
        questionLabeled "calphonsFolly.flip"
        causedBy attrs
        targets uncharted \lid -> do
          other <- cdOtherSide . toCardDef <$> getLocation lid
          flipOver lid
          if other `elem` map (Just . toCardCode) withForcedEffect
            then push $ UseCardAbility lead (LocationSource lid) 1 []
            else for_ active \a -> do
              placeLocation a StagingArea
              makeActive lid
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      x <- sum <$> (traverse getLocationThreat =<< select IsActiveLocation)
      when (x > 0) $ enemyGetsAttack (CardIdSource cid) eid x
      pure t
    _ -> CalphonsFolly <$> runMessage msg attrs
