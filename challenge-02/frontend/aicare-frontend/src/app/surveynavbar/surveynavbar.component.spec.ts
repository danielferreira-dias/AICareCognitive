import { ComponentFixture, TestBed } from '@angular/core/testing';

import { SurveynavbarComponent } from './surveynavbar.component';

describe('SurveynavbarComponent', () => {
  let component: SurveynavbarComponent;
  let fixture: ComponentFixture<SurveynavbarComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [SurveynavbarComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(SurveynavbarComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
